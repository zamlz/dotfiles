import os

def settings( **kwargs ):
    return {
        'interpreter_path': os.environ['VIRTUAL_ENV']
    }
