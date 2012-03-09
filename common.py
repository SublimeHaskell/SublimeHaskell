import subprocess

def call_and_wait(command, **popen_kwargs):
    """Run the specified command, block until it completes, and return
    the exit code, stdout, and stderr.
    Additional parameters to Popen can be specified as keyword parameters."""
    startupinfo = subprocess.STARTUPINFO()
    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    process = subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        startupinfo=startupinfo,
        **popen_kwargs)
    stdout, stderr = process.communicate()
    exit_code = process.wait()
    return (exit_code, stdout, stderr)

def log(message):
    print('Sublime Haskell: ' + message)
