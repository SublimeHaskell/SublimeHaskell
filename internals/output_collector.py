import sublime
import sublime_plugin

import threading
import subprocess

if int(sublime.version()) < 3000:
    from internals.proc_helper import ProcHelper
    from sublime_haskell_common import output_text
else:
    from SublimeHaskell.internals.proc_helper import ProcHelper
    from SublimeHaskell.sublime_haskell_common import output_text


class OutputCollector(threading.Thread):
    """Show a process' stdout in an output window. stderr can be tied to stdout; if not, stderr is collected and
    can be retrieved via the OutputCollector.wait() method."""

    def __init__(self, panel, cmdargs, tie_stderr = True, **popen_args):
        super(OutputCollector, self).__init__(name = 'output-collector')
        self.panel = panel
        self.prochelp = ProcHelper(cmdargs, stderr = subprocess.STDOUT if tie_stderr else subprocess.PIPE, **popen_args)
        self.exit_code = -1
        self.stderr_thread = None
        if not tie_stderr and self.prochelp.process is not None:
            # Need to spark up a thread to collect stderr's output
            self.stderr_thread = ErrorCollector(self.panel, self.prochelp.process.stderr)
            self.stderr_thread.start()
        self.stderr_lines = ""

    def wait(self):
        # Wait for this thread to complete...
        self.join()
        return (self.exit_code, self.stderr_lines)

    def run(self):
        if self.prochelp.process is None:
            self.panel.run_command('insert', { 'characters': self.prochelp.process_err })
            return

        panel_settings = self.panel.settings()

        autoindent = panel_settings.get('auto_indent')
        roflag = self.panel.is_read_only()

        panel_settings.set('auto_indent', False)
        self.panel.set_read_only(False)

        p = self.prochelp
        try:
            for l in p.process.stdout:
                self.panel.run_command('insert', {'characters': l})
        except ValueError as ve:
            pass

        self.exit_code = p.process.wait()
        self.panel.run_command('insert', {'characters': '\n\nExit code: {0}'.format(self.exit_code)})
        if self.stderr_thread is not None:
            self.stderr_thread.join()
            self.stderr_lines = ''.join(self.stderr_thread.lines)

        panel_settings.set('auto_indent', autoindent)
        self.panel.set_read_only(roflag)
        p.cleanup()

class ErrorCollector(threading.Thread):
    """stderr file object output collector. This accumulates lines into a list and also sends each line to
    a designated output panel."""
    def __init__(self, panel, stderr_fo):
        super(ErrorCollector, self).__init__(name = 'stderr-collector')
        self.panel = panel
        self.stderr_fo = stderr_fo
        self.lines = []

    def stop(self):
        self.terminate.set()

    def run(self):
        try:
            for l in self.stderr_fo:
                if len(l) > 0:
                    self.lines.append(l)
                    if self.panel is not None:
                        self.panel.run_command('insert', {'characters': l})
        except ValueError as ve:
            # Exit the loop, since we can't read any more from the file object
            pass


class DescriptorDrain(threading.Thread):
    """Continually running thread that drains a Python file, sending everything read to stdout (which in ST's case
    is a logging object)"""

    ### This really belongs in sublime_haskell_common. But, since that module gets loaded later than this one OR
    ### it gets reloaded, you end up with the dreaded super() TypeError.
    def __init__(self, label, fd):
        super(DescriptorDrain, self).__init__(name = 'drain-' + label)
        self.label = label
        self.fd = fd
        self.stop_me = threading.Event()

    def run(self):
        while not self.stop_me.is_set():
            l = self.fd.readline().rstrip()
            print('<{0}> {1}'.format(self.label, l))

    def stop(self):
        self.stop_me.set()
