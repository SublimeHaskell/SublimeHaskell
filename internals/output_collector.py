import sublime
import sublime_plugin

import faulthandler
import io
import threading
import subprocess

if int(sublime.version()) < 3000:
    from internals.proc_helper import ProcHelper
    from sublime_haskell_common import output_text
else:
    import SublimeHaskell.internals.proc_helper as ProcHelper
    import SublimeHaskell.internals.utils as Utils


class OutputCollector(object):
    """Show a process' stdout in an output window. stderr can be tied to stdout; if not, stderr is
    collected and can be retrieved via the OutputCollector.wait() method."""

    def __init__(self, panel, cmdargs, tie_stderr=True, **popen_args):
        stderr_flag = subprocess.STDOUT if tie_stderr else subprocess.PIPE
        panel_settings = panel.settings()

        self.prochelp = ProcHelper.ProcHelper(cmdargs, stderr=stderr_flag, **popen_args)
        self.panel = panel
        self.lines = []
        self.exit_code = -1
        self.stdout_collector = None
        self.stderr_collector = None
        self.autoindent = panel_settings.get('auto_indent')
        self.roflag = self.panel.is_read_only()

        if self.prochelp.process is not None:
            lines_lock = threading.RLock()
            self.stdout_collector = FileObjectCollector("stdout-collector", panel, lines_lock,
                                                        self.lines, self.prochelp.process.stdout)
            self.stdout_collector.start()
            if not tie_stderr:
                self.stderr_collector = FileObjectCollector("stderr-collector", panel, lines_lock,
                                                            self.lines, self.prochelp.process.stderr)
                self.stderr_collector.start()

        panel_settings.set('auto_indent', False)
        self.panel.set_read_only(False)

    def wait(self):
        if self.prochelp is not None:
            # Wait for process and threads to complete...
            self.exit_code = self.prochelp.process.wait()

            if self.stdout_collector is not None:
                self.stdout_collector.join()
            if self.stderr_collector is not None:
                self.stderr_collector.join()

            exit_msg = '\n\nExit code: {0}\n\n'.format(self.exit_code) if self.exit_code != 0 else ''
            self.panel.run_command('insert', {'characters': exit_msg})
            self.prochelp.cleanup()
        else:
            self.lines = self.prochelp.process_err
            self.panel.run_command('insert', {'characters': self.prochelp.process_err})

            panel_settings.set('auto_indent', autoindent)
            self.panel.set_read_only(roflag)

        panel_settings = self.panel.settings()
        panel_settings.set('auto_indent', self.autoindent)
        self.panel.set_read_only(self.roflag)

        return (self.exit_code, ''.join(self.lines))

class FileObjectCollector(threading.Thread):
    """stderr file object output collector. This accumulates lines into a list and also sends
    each line to a designated output panel."""
    def __init__(self, name, panel, lines_lock, lines, fobject):
        super(FileObjectCollector, self).__init__(name=name)

        self.panel = panel
        self.lines_lock = lines_lock
        self.lines = lines
        self.fobject = fobject

    def run(self):
        try:
            for l in io.TextIOWrapper(self.fobject, encoding="utf-8"):
                # l = Utils.decode_bytes(l)
                with self.lines_lock:
                    self.lines.append(l)
                self.panel.run_command('insert', {'characters': l})

        except ValueError as ve:
            # Uncomment for debugging...
            # print("ValueError {0}".format(ve))

            # Exit the loop, since we can't read any more from the file object
            pass

class DescriptorDrain(threading.Thread):
    """Continually running thread that drains a Python file, sending everything read to stdout
    (which in ST's case is a logging object)"""

    ### This really belongs in sublime_haskell_common. But, since that module gets loaded later
    ### than this one OR it gets reloaded, you end up with the dreaded super() TypeError.
    def __init__(self, label, fd):
        super(DescriptorDrain, self).__init__(name='drain-' + label)
        self.label = label
        self.fd = fd
        self.stop_me = threading.Event()

    def run(self):
        while not self.stop_me.is_set():
            l = Utils.decode_bytes(self.fd.readline()).rstrip()
            print('<{0}> {1}'.format(self.label, l))

    def stop(self):
        self.stop_me.set()
