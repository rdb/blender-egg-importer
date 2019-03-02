This is an importer for Blender that can import .egg files.  It is designed to
be interoperable with YABEE and other Blender to Panda3D exporters, and aims
to preserve the hierarchical structure of the models.

In combination with the improved version of bam2egg available in development
builds of Panda3D, this also provides a reliable path to import .bam files
that are generated by egg2bam.

## Installation

This is a Blender addon that does not require a Panda3D installation.

Clone this repository in your Blender addons directory, or download as a .zip
file and use "Install from File" in the Blender User Preferences window.

## Normals

The importer attempts to create a completely accurate representation of the
model in Blender.  This means that it will only avoid importing a custom set
of normal data if it can prove that Blender would generate the same normals.

In many cases it will not be able to prove this, so it may unnecessarily
create Split Normals Data for your mesh.  This may make the normals of the
mesh look funny in Blender after editing or animation.

To disable this, select the Mesh data tab, and under "Geometry Data", click
"Clear Custom Split Normals Data".  You may also want to disable "Auto Smooth"
if desired.

## Animations

Skeletal animations are supported, experimentally.  A model needs to be loaded
with all of its animations in the same import run.  If the animation exists in
a separate egg file, this means that the model file and all of its animation
files will need to be selected in the file selector dialog.

Animations are not automatically bound to their corresponding armature.  To
see an imported animation, you will need to select the armature, go to the
Dope Sheet window, bring up the Action Editor, and select the appropriate
action.

## External references

By default, external references are imported as Empty with a "file" game
property, which is a special property that is translated back into the
appropriate syntax by YABEE.

If the optional checkbox titled "Load external instances" is ticked, each of
the referenced files will be loaded as a separate scene, grouped, and
instanced to the Empty using the DupliGroup functionality in Blender.  These
are ignored by YABEE, so the external references are preserved.

Beware that the required syntax for external references is rather restrictive.
This is what an external reference generated by YABEE looks like, which is
supported:

```
<Group> something {
  <Transform> { ... }
  <Instance> { <File> { filename.egg } }
}
```

The format used by flt2egg is also supported, with the transform exclusively
inside the instance:

```
<Group> something {
  <Instance> {
    <Transform> { ... }
    <File> { filename.egg }
  }
}
```

If you have files containing external references in a different configuration,
please create an issue on GitHub and attach the model in question.

Recursive references are not supported at present.

## Features

Most features supported by bam2egg are implemented.  In particular, however,
animation support is not yet finished.  Many other features still need to be
tested.

Supported features:
- .egg.pz and .egg.gz files
- Basic geometry, incl. tristrips and trifans
- Custom normals
- Vertex colors
- All transform types
- Materials
- Textures
- Tags (as game properties)
- Collide and ObjectType (as game properties)
- Collision masks (as game properties)
- Coordinate system conversion
- Multiple UV coordinate sets
- Armatures, skinning
- Default animation pose
- Bone animations
- Morph targets
- YABEE/flt2egg-style external references ('file' game property)

Yet to do:
- Improve performance of parser
- Lines, patches
- NURBS surfaces and curves
- Level of detail
- Support recursive external references

## Limitations

Panda3D does not have a concept of "bone length"; the importer does some
clever tricks to guess what the original bone length might have been, but some
bones may look wrong.  This should not negatively affect skinning, however.

Shear animations are not supported at present.  If you have a file that
contains shear animations, please send it to me.

If you find any bugs, please open a bug report on the issue tracker, and
include the egg file that loaded incorrectly.
