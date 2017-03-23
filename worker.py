import threading

import sublime

try:
    import queue
except ImportError:
    import Queue as queue

if int(sublime.version()) < 3000:
    import internals.logging as Logging
else:
    import SublimeHaskell.internals.logging as Logging

# Background worker
class Worker(threading.Thread):
    def __init__(self):
        super(Worker, self).__init__()
        self.jobs = queue.Queue()

    def run(self):
        while True:
            name, fn, args, kwargs = self.jobs.get()
            try:
                fn(*args, **kwargs)
            except Exception as e:
                Logging.log('worker: job {0} fails with {1}'.format(name, e), Logging.LOG_DEBUG)

    def async(self, name, fn, *args, **kwargs):
        self.jobs.put((name, fn, args, kwargs))

worker = None


def run_async(name, fn, *args, **kwargs):
    global worker
    if not worker or not worker.is_alive():
        worker = Worker()
        worker.start()
    worker.async(name, fn, *args, **kwargs)
