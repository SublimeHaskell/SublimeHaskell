
import traceback

import SublimeHaskell.internals.logging as Logging

def call_callback(fn, *args, **kwargs):
    name = kwargs.get('name')
    if name:
        del kwargs['name']
    try:
        if fn is not None:
            fn(*args, **kwargs)
    except:
        Logging.log('callback \'{0}\' throws exception, see console window traceback'.format(name or '<unnamed>'),
                    Logging.LOG_ERROR)
        print(traceback.format_exc())
