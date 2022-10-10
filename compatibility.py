import bpy

def make_annotations(cls):
    # We do not need property annotations in <2.93
    if bpy.app.version < (2, 93):
        return cls

    # Get all the class properties for which have _PropertyDeferred
    props = {k: v for k, v in cls.__dict__.items() if isinstance(v, bpy.props._PropertyDeferred)}
    if props:
        # Setup annotations if the class doesn't have them
        if '__annotations__' not in cls.__dict__:
            setattr(cls, '__annotations__', {})
        annotations = cls.__dict__['__annotations__']

        # Generate annotations for these properties
        for k, v in props.items():
            annotations[k] = v
            delattr(cls, k)

    return cls
