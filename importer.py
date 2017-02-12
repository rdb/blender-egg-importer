""" This files contains classes that interpret the .egg object model and write
out the appropriate Blender structures. """

from .eggparser import parse_number

import sys, os
import bpy, bmesh
from mathutils import Matrix, Vector
from math import radians

DEFAULT_UV_NAME = "UVMap"


class EggContext:
    def __init__(self):
        self.vertex_pools = {}
        self.materials = {}
        self.textures = {}
        self.search_dir = None


class EggRenderMode:
    """ Accumulates render state attributes. """

    def __init__(self, parent):
        if parent is None:
            self.alpha_mode = None
            self.depth_write_mode = None
            self.depth_test_mode = None
            self.visibility_mode = None
            self.depth_offset = None
            self.draw_order = None
            self.bin = None
        else:
            self.alpha_mode = parent.alpha_mode
            self.depth_write_mode = parent.depth_write_mode
            self.depth_test_mode = parent.depth_test_mode
            self.visibility_mode = parent.visibility_mode
            self.depth_offset = parent.depth_offset
            self.draw_order = parent.draw_order
            self.bin = parent.bin

    def parse_scalar(self, name, value):
        pass


class EggMaterial:
    __slots__ = 'name', 'base', 'diff', 'amb', 'emit', 'spec', 'shininess', 'roughness', 'metallic', 'ior', 'materials'
    def __init__(self, name):
        self.name = name

        self.base = [1, 1, 1, 1]
        self.diff = [1, 1, 1, 1]
        self.amb = [1, 1, 1, 1]
        self.emit = [0, 0, 0, 1]
        self.spec = [0, 0, 0, 1]
        self.shininess = 0
        self.roughness = 1
        self.metallic = 0
        self.ior = 1

        self.materials = {}

    def begin_child(self, context, type, name, values):
        if type.upper() in ('SCALAR', 'CHAR*'):
            name = name.lower()
            value = parse_number(values[0])

            if name == 'baser':
                self.base[0] = value
            elif name == 'baseg':
                self.base[1] = value
            elif name == 'baseb':
                self.base[2] = value
            elif name == 'basea':
                self.base[3] = value
            elif name == 'diffr':
                self.diff[0] = value
            elif name == 'diffg':
                self.diff[1] = value
            elif name == 'diffb':
                self.diff[2] = value
            elif name == 'diffa':
                self.diff[3] = value
            elif name == 'ambr':
                self.amb[0] = value
            elif name == 'ambg':
                self.amb[1] = value
            elif name == 'ambb':
                self.amb[2] = value
            elif name == 'amba':
                self.amb[3] = value
            elif name == 'emitr':
                self.emit[0] = value
            elif name == 'emitg':
                self.emit[1] = value
            elif name == 'emitb':
                self.emit[2] = value
            elif name == 'emita':
                self.emit[3] = value
            elif name == 'specr':
                self.spec[0] = value
            elif name == 'specg':
                self.spec[1] = value
            elif name == 'specb':
                self.spec[2] = value
            elif name == 'speca':
                self.spec[3] = value
            elif name == 'shininess':
                self.shininess = value
            elif name == 'roughness':
                self.roughness = value
            elif name == 'metallic':
                self.metallic = value
            elif name == 'ior':
                self.ior = value

    def get_material(self, textures, color, bface, alpha):
        """ Returns the material for the indicated primitive. """

        if len(textures) == 0 and self is EggPrimitive.default_material and not bface and not alpha:
            return None

        key = (tuple(textures), color, bface, alpha)
        if key in self.materials:
            return self.materials[key]

        bmat = bpy.data.materials.new(self.name)
        bmat.diffuse_intensity = 1.0
        bmat.specular_intensity = 1.0
        bmat.specular_hardness = self.shininess * 2

        if not any(self.spec) and not any(self.diff) and not any(self.amb):
            # This is YABEE's way of making a shadeless material.
            bmat.use_shadeless = True
            bmat.diffuse_color = self.emit[:3]
        else:
            bmat.diffuse_color = self.diff[:3]
            bmat.specular_color = self.spec[:3]
            bmat.specular_alpha = self.spec[3]

            # Blender only supports one ambient value, so we average it.
            bmat.ambient = (self.amb[0] + self.amb[1] + self.amb[2]) / 3

            # Same for emission, except that it specifes the emission as a product
            # of the diffuse color, so we have to divide it.
            if any(bmat.diffuse_color):
                bmat.emit = sum(self.emit[:3]) / sum(bmat.diffuse_color)

        if alpha:
            alpha = alpha.lower()
            if alpha == 'off':
                bmat.game_settings.alpha_blend = 'OPAQUE'
            elif alpha.startswith('ms'):
                bmat.game_settings.alpha_blend = 'ALPHA_ANTIALIASING'
            elif alpha == 'binary':
                bmat.game_settings.alpha_blend = 'CLIP'
            else:
                bmat.game_settings.alpha_blend = 'ALPHA'

        bmat.game_settings.use_backface_culling = not bface

        for i, texture in enumerate(textures):
            slot = bmat.texture_slots.add()
            slot.texture = texture.texture
            slot.uv_layer = texture.uv_name or "UVMap"

            # Convert the Panda texture transform to the equivalent Blender
            # transform.  Ignores rotation, shear, or axis remap.
            m = texture.matrix
            if m is not None:
                pos = Vector((m[0][3], m[1][3], m[2][3]))
                slot.scale = (m[0][0], m[1][1], m[2][2])
                slot.offset = pos + (slot.scale - Vector((1, 1, 1))) * 0.5

            if texture.envtype == 'modulate':
                slot.use_map_color_diffuse = True
            elif texture.envtype == 'normal':
                slot.use_map_normal = True
            elif texture.envtype == 'glow':
                slot.use_map_emit = True
            elif texture.envtype == 'gloss':
                slot.use_map_specular = True

        self.materials[key] = bmat
        return bmat


class EggTexture:
    def __init__(self, name, path, search_dir=None):
        self.texture = bpy.data.textures.new(name, 'IMAGE')
        self.search_dir = search_dir
        self.envtype = 'modulate'
        self.uv_name = None
        self.matrix = None

        if sys.platform == 'win32':
            # Convert an absolute Panda-style path to a Windows path.
            if len(path) > 3 and path[0] == '/' and path[2] == '/':
                path = path.replace('/', '\\')
                path = path[1].upper() + ':' + path[2:]

        path = path.replace('/', '\\')

        # If it's a relative path, search in the location of the .egg first.
        if not os.path.isabs(path) and self.search_dir and os.path.exists(os.path.join(self.search_dir, path)):
            path = os.path.join(self.search_dir, path)
            path = path.replace('\\.\\', '\\')
            self.texture.image = bpy.data.images.load(path)
            #self.texture.image.filepath = path
        else:
            # Just load it with the original path.  If it fails, let the user
            # deal with the failure.
            self.texture.image = bpy.data.images.load(path)

    def begin_child(self, context, type, name, values):
        type = type.upper()

        if type in ('SCALAR', 'CHAR*'):
            name = name.lower()

            if name == 'wrap':
                value = values[0].lower()
                if value == 'repeat':
                    self.texture.extension = 'REPEAT'
                elif value == 'clamp':
                    self.texture.extension = 'EXTEND'
                elif value in ('border_color', 'border-color'):
                    self.texture.extension = 'CLIP'

            elif name == 'envtype':
                self.envtype = values[0].lower()
                if self.envtype == 'normal':
                    self.texture.use_normal_map = True

            elif name == 'minfilter':
                if 'mipmap' in values[0].lower():
                    self.texture.use_mipmap = True

            elif name == 'alpha':
                if values[0].lower() == 'premultiplied':
                    self.texture.image.alpha_mode = 'PREMUL'

            elif name == 'uv_name' or name == 'uv-name':
                self.uv_name = values[0]

        elif type == 'TRANSFORM':
            return EggTransform()

    def end_child(self, context, type, name, child):
        if isinstance(child, EggTransform):
            if self.matrix is not None:
                self.matrix *= child.matrix
            else:
                self.matrix = child.matrix


class EggTransform:
    __slots__ = 'matrix',

    def __init__(self):
        self.matrix = Matrix()

    def begin_child(self, context, type, name, values):
        v = [parse_number(v) for v in values]

        type = type.upper()
        if type == 'TRANSLATE':
            if len(values) == 2:
                self.matrix = Matrix.Translation(v + [0]) * self.matrix
            else:
                self.matrix = Matrix.Translation(v) * self.matrix

        elif type == 'ROTATE':
            self.matrix = Matrix.Rotation(radians(v[0]), 4, v[1:] or (0, 0, 1)) * self.matrix

        elif type == 'ROTX':
            self.matrix = Matrix.Rotation(radians(v[0]), 4, 'X') * self.matrix

        elif type == 'ROTY':
            self.matrix = Matrix.Rotation(radians(v[0]), 4, 'Y') * self.matrix

        elif type == 'ROTZ':
            self.matrix = Matrix.Rotation(radians(v[0]), 4, 'Z') * self.matrix

        elif type == 'SCALE':
            if len(v) == 1:
                x = y = z = v[0]
            elif len(v) == 2:
                x, y = v
                z = 1
            else:
                x, y, z = v
            self.matrix = Matrix(((x, 0, 0, 0), (0, y, 0, 0), (0, 0, z, 0), (0, 0, 0, 1))) * self.matrix

        elif type == 'MATRIX3':
            self.matrix = Matrix(((v[0], v[3],  0.0, v[6]),
                                  (v[1], v[4],  0.0, v[7]),
                                  ( 0.0,  0.0,  1.0,  0.0),
                                  (v[2], v[5],  0.0, v[8]))) * self.matrix

        elif type == 'MATRIX4':
            self.matrix = Matrix(((v[0], v[4], v[8], v[12]),
                                  (v[1], v[5], v[9], v[13]),
                                  (v[2], v[6], v[10], v[14]),
                                  (v[3], v[7], v[11], v[15]))) * self.matrix


class EggVertex:
    __slots__ = 'pos', 'normal', 'color', 'uv_map', 'aux_map', 'dxyzs'

    def __init__(self, pos):
        self.pos = pos
        self.normal = (0, 0, 0)
        self.uv_map = {}
        self.aux_map = {}
        self.dxyzs = {}

    def begin_child(self, context, type, name, values):
        if type.upper() == 'NORMAL':
            self.normal = [parse_number(v) for v in values]

        elif type.upper() == 'RGBA':
            self.color = [parse_number(v) for v in values]

        elif type.upper() == 'UV':
            self.uv_map[name or DEFAULT_UV_NAME] = [parse_number(v) for v in values]

        elif type.upper() == 'AUX':
            self.aux_map[name] = [parse_number(v) for v in values]


class EggVertexPool:
    def __init__(self):
        self._vertices = []

    def __getitem__(self, index):
        return self._vertices[index]

    def begin_child(self, context, type, name, values):
        if type.upper() != 'VERTEX':
            assert False

        return EggVertex((parse_number(v) for v in values))

    def end_child(self, context, type, name, vertex):
        verts = self._vertices
        if name:
            # Add the vertex at the requested index.
            num_vertices = len(verts)
            index = int(name)
            if index < num_vertices:
                assert verts[index] is None
                vertex = EggVertex()
                self._vertices[index] = vertex
            else:
                if index != num_vertices:
                    verts.extend([None] * (index - num_vertices))
                verts.append(vertex)
        else:
            verts.append(vertex)


class EggPrimitive:
    __slots__ = 'indices', 'pool', 'material', 'textures', 'normal', 'color', 'bface', 'alpha', 'visibility'

    default_material = EggMaterial("default")

    def __init__(self):
        self.material = EggPrimitive.default_material
        self.color = (1, 1, 1, 1)
        self.normal = None
        self.bface = False
        self.alpha = None
        self.visibility = None
        self.textures = []

    def begin_child(self, context, type, name, values):
        type = type.upper()

        if type == 'VERTEXREF':
            self.indices = tuple(int(v) for v in values)
            return self # To catch the <Ref>

        elif type == 'REF':
            self.pool = values[0]

        elif type == 'SCALAR' or type == 'CHAR*':
            name = name.lower()

        elif type == 'TREF':
            self.textures.append(context.textures[values[0]])

        elif type == 'MREF':
            self.material = context.materials[values[0]]


class EggGroupNode:
    def __init__(self):
        self.children = []

    def begin_child(self, context, type, name, values):
        type = type.upper()

        if type == 'COORDINATESYSTEM':
            pass
        elif type == 'TEXTURE':
            tex = EggTexture(name, values[0], search_dir=context.search_dir)
            context.textures[name] = tex
            return tex
        elif type == 'MATERIAL':
            mat = EggMaterial(name)
            context.materials[name] = mat
            return mat
        elif type == 'VERTEXPOOL':
            vpool = EggVertexPool()
            context.vertex_pools[name] = vpool
            return vpool
        elif type in ('GROUP', 'INSTANCE'):
            return EggGroup(name, parent=self)
        elif type == 'JOINT':
            return EggJoint(name, parent=self)
        elif type == 'POLYGON':
            return EggPrimitive()
        elif type == 'TRIANGLEFAN':
            pass
        elif type == 'TRIANGLESTRIP':
            pass
        elif type == 'PATCH':
            pass
        elif type == 'POINTLIGHT':
            pass
        elif type == 'LINE':
            pass
        elif type == 'NURBSSURFACE':
            pass
        elif type == 'NURBSCURVE':
            pass
        elif type == 'TABLE':
            pass
        elif type == 'ANIMPRELOAD':
            pass

    def end_child(self, context, type, name, child):
        if isinstance(child, EggGroupNode):
            self.children.append(child)

    def build_tree(self, parent=None, inv_matrix=None):
        for child in self.children:
            child.build_tree(parent)

    def build_armature(self, *args, **kwargs):
        for child in self.children:
            child.build_armature(*args, **kwargs)


class EggGroup(EggGroupNode):
    def __init__(self, name, parent):
        EggGroupNode.__init__(self)
        self.name = name
        self.properties = {}

        self.vertices = {}

        self.matrix = None
        self.bmesh = None
        self.materials = []
        self.dart = False

    def get_bvert(self, vert):
        if vert in self.vertices:
            return self.vertices[vert]

        bvert = self.bmesh.verts.new(vert.pos)
        bvert.normal = vert.normal
        self.vertices[vert] = bvert
        return bvert

    def begin_child(self, context, type, name, values):
        type = type.upper()

        #if type in ('SCALAR', 'CHAR*', 'BILLBOARD', 'DCS', 'DART', 'SWITCH', 'OBJECTTYPE', 'TAG', 'MODEL', 'TEXLIST', 'REF'):

        if type in ('SCALAR', 'CHAR*'):
            name = name.lower().replace('_', '-')

            # YABEE recognizes these scalars as game properties.
            if name in ('collide-mask', 'from-collide-mask', 'into-collide-mask', 'bin', 'draw-order'):
                self.properties[name] = values[0]

        elif type in ('COLLIDE', 'OBJECTTYPE'):
            self.properties[type] = values[0]

        elif type == 'TAG':
            # Odd, but the reference .egg parser really intentionally joins
            # multiple values using newlines when parsing the <Tag> body.
            self.properties[name] = '\n'.join(values)

        elif type == 'TRANSFORM':
            return EggTransform()

        elif type == 'DART':
            # This indicates the root of an animated model.
            # Do we need to consider other dart types?
            self.dart = values[0].strip().lower() not in ('0', 'none')

        return EggGroupNode.begin_child(self, context, type, name, values)

    def end_child(self, context, type, name, child):
        if isinstance(child, EggTransform):
            if self.matrix is not None:
                self.matrix *= child.matrix
            else:
                self.matrix = child.matrix

        elif isinstance(child, EggPrimitive):
            if self.bmesh is None:
                self.bmesh = bmesh.new()
                self.tex_layer = self.bmesh.faces.layers.tex.verify()
            vpool = context.vertex_pools[child.pool]

            # Check the indices for duplicates, or Blender will freak out.
            verts = []
            uvs = []
            last = child.indices[-1]
            for index in child.indices:
                if index != last:
                    vert = vpool[index]
                    verts.append(self.get_bvert(vert))
                    uvs.append(vert.uv_map.get('UVMap'))
                    last = index

            face = self.bmesh.faces.new(verts)

            # (Arbitrarily) assign the first texture as UV image.
            if child.textures:
                face[self.tex_layer].image = child.textures[0].texture.image

            # Assign UVs.
            if uvs.count(None) < len(uvs):
                uv_layer = self.bmesh.loops.layers.uv.verify()
                for i, loop in enumerate(face.loops):
                    assert verts[i] == loop.vert
                    if uvs[i]:
                        loop[uv_layer].uv = uvs[i][:2]

            # Check if we already have a material for this combination.
            bmat = child.material.get_material(child.textures, child.color, child.bface, child.alpha)
            if bmat in self.materials:
                index = self.materials.index(bmat)
            else:
                index = len(self.materials)
                self.materials.append(bmat)
            face.material_index = index

        return EggGroupNode.end_child(self, context, type, name, child)

    def build_tree(self, parent, inv_matrix=None):
        """ Walks the hierarchy of groups and builds the Blender object graph.
        This needs to happen after adding all the children so that we fully
        know the parent-child hierarchy and transforms. """

        data = None
        if self.bmesh:
            data = bpy.data.meshes.new(self.name)
            for mat in self.materials:
                data.materials.append(mat)
            self.bmesh.to_mesh(data)
            self.bmesh = None

        elif self.dart:
            data = bpy.data.armatures.new(self.name)

        object = bpy.data.objects.new(self.name, data)
        object.parent = parent

        if self.matrix:
            object.matrix_basis = self.matrix
            if inv_matrix:
                inv_matrix = self.matrix.inverted() * inv_matrix
            else:
                inv_matrix = self.matrix.inverted()

        # The .egg format specifies vertex data in global space, so we have to
        # transform the object by its inverse matrix to compensate for that.
        if inv_matrix and object.type == 'MESH':
            object.data.transform(inv_matrix)

        bpy.context.scene.objects.link(object)

        # Awkward, but it seems there's no other way to set a game property
        # or create bones.
        if self.properties or self.dart:
            active = bpy.context.scene.objects.active
            bpy.context.scene.objects.active = object
            for name, value in self.properties.items():
                bpy.ops.object.game_property_new(type='STRING', name=name)
                object.game.properties[name].value = value

            if self.dart:
                #bpy.context.scene.update()
                bpy.ops.object.mode_set(mode='EDIT')
                self.build_armature(data, None, Matrix())
                bpy.ops.object.mode_set(mode='OBJECT')

            bpy.context.scene.objects.active = active

        for child in self.children:
            child.build_tree(object, inv_matrix)

        return object


class EggJoint(EggGroup):
    def build_armature(self, armature, parent, matrix):
        matrix *= self.matrix

        bone = armature.edit_bones.new(self.name)
        bone.parent = parent
        bone.tail = Vector((0, 1, 0))
        bone.matrix = matrix
        bone.use_connect = True

        EggGroupNode.build_armature(self, armature, bone, matrix)
