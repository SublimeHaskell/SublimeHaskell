import traceback
import threading

try:
    import queue
except ImportError:
    import Queue as queue

import SublimeHaskell.internals.logging as Logging

# Background worker
class Worker(threading.Thread):
    def __init__(self):
        super().__init__()
        self.jobs = queue.Queue()

    def run(self):
        while True:
            name, worker_fn, args, kwargs = self.jobs.get()
            try:
                worker_fn(*args, **kwargs)
            except:
                Logging.log('worker: job {0} failws, see console window traceback'.format(name), Logging.LOG_DEBUG)
                traceback.print_exc()

    def async(self, name, worker_fn, *args, **kwargs):
        self.jobs.put((name, worker_fn, args, kwargs))

WORKER_QUEUE = None


def run_async(name, worker_fn, *args, **kwargs):
    global WORKER_QUEUE
    if not WORKER_QUEUE or not WORKER_QUEUE.is_alive():
        WORKER_QUEUE = Worker()
        WORKER_QUEUE.start()
    WORKER_QUEUE.async(name, worker_fn, *args, **kwargs)
