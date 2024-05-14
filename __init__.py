bl_info = {
    "name": "Import Panda3D .egg models",
    "author": "rdb",
    "version": (2, 3),
    "blender": (2, 80, 0),
    "location": "File > Import > Panda3D (.egg)",
    "description": "",
    "warning": "",
    "category": "Import-Export",
}

import bpy
if bpy.app.version < (2, 80):
    bl_info["blender"] = (2, 74, 0) # Needed for normals_split_custom_set


if "loaded" in locals():
    import imp
    imp.reload(eggparser)
    imp.reload(importer)
else:
    from . import eggparser
    from . import importer

loaded = True

import os.path
import bpy.types
from bpy import props
from bpy_extras.io_utils import ImportHelper


class IMPORT_OT_egg(bpy.types.Operator, ImportHelper):
    """Import .egg Operator"""
    bl_idname = "import_scene.egg"
    bl_label = "Import .egg"
    bl_description = "Import a Panda3D .egg file"
    bl_options = {'REGISTER', 'UNDO'}

    filename_ext = ".egg"
    filter_glob = props.StringProperty(default="*.egg;*.egg.pz;*.egg.gz", options={'HIDDEN'})

    directory = props.StringProperty(name="Directory", options={'HIDDEN'})
    files = props.CollectionProperty(type=bpy.types.OperatorFileListElement, options={'HIDDEN'})

    load_external = props.BoolProperty(name="Load external references", description="Loads other .egg files referenced by this file as separate scenes, and instantiates them using DupliGroups.")
    auto_bind = props.BoolProperty(name="Auto bind", default=True, description="Automatically tries to bind actions to armatures.")

    def execute(self, context):
        context = importer.EggContext()
        context.info = lambda msg: self.report({'INFO'}, context.prefix_message(msg))
        context.warn = lambda msg: self.report({'WARNING'}, context.prefix_message(msg))
        context.error = lambda msg: self.report({'ERROR'}, context.prefix_message(msg))
        context.search_dir = self.directory
        roots = []

        for file in self.files:
            path = os.path.join(self.directory, file.name)
            root = context.read_file(path)
            roots.append(root)

        for root in roots:
            root.build_tree(context)
        context.assign_vertex_groups()

        if self.load_external:
            context.load_external_references()

        if self.auto_bind:
            context.auto_bind()

        context.final_report()
        return {'FINISHED'}

    def invoke(self, context, event):
        wm = context.window_manager
        wm.fileselect_add(self)
        return {'RUNNING_MODAL'}

    def draw(self, context):
        layout = self.layout
        row = layout.row()
        row.prop(self, "load_external")
        row = layout.row()
        row.prop(self, "auto_bind")


def menu_func(self, context):
    self.layout.operator(IMPORT_OT_egg.bl_idname, text="Panda3D (.egg)")

def register():
    bpy.utils.register_class(IMPORT_OT_egg)

    if bpy.app.version >= (2, 80):
        bpy.types.TOPBAR_MT_file_import.append(menu_func)
    else:
        bpy.types.INFO_MT_file_import.append(menu_func)

def unregister():
    if bpy.app.version >= (2, 80):
        bpy.types.TOPBAR_MT_file_import.remove(menu_func)
    else:
        bpy.types.INFO_MT_file_import.remove(menu_func)

    bpy.utils.unregister_class(IMPORT_OT_egg)

if __name__ == "__main__":
    register()
