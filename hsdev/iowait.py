# Copyright (C) 2011, 2013  Andrea Corbellini
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Platform-independent module for I/O completion events.

This module provides objects to watch file descriptors for I/O events. The
objects have all the same interface and work in the same way in every platform.
The only limitation of this module is that, on Windows, it only works for
sockets.

This module defines the following classes:

* SelectIOWait -- based on the select() system call;
* PollIOWait -- based on poll();
* EPollIOWait -- based on epoll();
* KQueueIOWait -- based on kqueue().

In addition, IOWait is also defined. It is a reference to the best
implementation available for the current platform (for example, it references
EPollIOWait on Linux and KQueueIOWait on FreeBSD).

The functions poll(), epoll() and kqueue() are not available on all operating
systems, so not all the classes defined by this module can be used on every
platform. For this reason, it is recommended to always use IOWait instead of
referencing the other classes directly.

Every IOWait-like object in this module provide the following methods:

* watch(file[, read[, write]])
  watch(event)

  Register the given file object. The type of event to wait for can be
  specified using the boolean read and write parameters (both False by
  default).

  Alternatively, you can pass a IOEvent object instead of the three
  file, read, write parameters.

  If the file is already registered, just the type of event to wait for is
  changed. If neither read nor write is specified, ValueError is raised.

* unwatch(file)

  Remove the given file object from the list of registered files. If the file
  isn't registered, ValueError is raised.

* wait([timeout])

  Wait for the events registered using watch(). This function returns a list
  containing IOEvent objects.

  The optional timeout argument specifies the time-out as a float in seconds.
  If not specified or None, the function will block until at least an event is
  received.

  If no files are registered, ValueError is raised.

* get_watched()

  Return all the file objects registered using watch(). The value returned is a
  list in the same format: (file, read, write).

* clear()

  Remove all the registered file objects.

In addition, the attribute available is also defined. When True, the class can
be used without problems, else the class won't work. Its value varies depending
on the platform (for example, EPollIOWait.avaliable is True on Linux, but False
on all other platforms).

All IOWait-like objects are subclass of the AbstractIOWait abstract base class.

The IOEvent object is a named tuple exposing three fields: fileobj, read and
write. fileobj is either a file object or a file number. read and write are two
booleans that specify whether the file is ready to be read or written without
blocking.
"""

import abc
import select
import warnings
from collections import namedtuple

__version__ = '0.3'
__all__ = [
    'IOWait',
    'IOEvent',
    'SelectIOWait',
    'PollIOWait',
    'EPollIOWait',
    'KQueueIOWait',
]


class IOEvent(namedtuple('IOEvent', 'fileobj read write')):

    def __new__(cls, fileobj, read=False, write=False):
        return super().__new__(
            cls, fileobj, bool(read), bool(write))

    @classmethod
    def _from_watch_args(cls, fileobj, *args, **kwargs):
        if isinstance(fileobj, IOEvent):
            if args or kwargs:
                raise TypeError('when called with an IOEvent object, '
                                'watch() takes only one argument')
            event = fileobj
        else:
            event = cls(fileobj, *args, **kwargs)
        if not (event.read or event.write):
            raise ValueError('either read or write must be specified')
        return event


try:
    _ABC = abc.ABC
except AttributeError:
    # Python < 3.4
    _ABC = abc.ABCMeta('ABC', (object,), {})


class AbstractIOWait(_ABC):

    closed = False

    @abc.abstractmethod
    def watch(self, fileobj, read=False, write=False):
        self._ensure_open()

    @abc.abstractmethod
    def unwatch(self, fileobj):
        self._ensure_open()

    @abc.abstractmethod
    def wait(self, timeout=None):
        self._ensure_open()

    @abc.abstractmethod
    def get_watched(self):
        self._ensure_open()

    @abc.abstractmethod
    def clear(self):
        self._ensure_open()

    @abc.abstractmethod
    def close(self):
        self.closed = True

    def _ensure_open(self):
        if self.closed:
            raise ValueError('operation on closed object')

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback):
        self.close()

    def __del__(self):
        if not self.closed:
            try:
                warnings.warn('unclosed IOWait object %r' % self,
                              ResourceWarning, stacklevel=2)
            except NameError:
                # Python < 3.2.
                pass
            self.close()


class SelectIOWait(AbstractIOWait):
    """Implementation based on the select() system function."""

    # select() is assumed to be always available.
    available = True

    def __init__(self):
        self._rlist = []
        self._wlist = []
        self._files_map = {}

    def watch(self, *args, **kwargs):
        super().watch(*args, **kwargs)
        event = IOEvent._from_watch_args(*args, **kwargs)
        fileobj, read, write = event

        if hasattr(fileobj, 'fileno'):
            fileno = fileobj.fileno()
        else:
            fileno = int(fileobj)

        files_map = self._files_map
        try:
            old_read, old_write = files_map[fileno][1:]
        except KeyError:
            # This is the first time this file is registered.
            if read:
                self._rlist.append(fileno)
            if write:
                self._wlist.append(fileno)
        else:
            # The file has already been registered.
            if read:
                if not old_read:
                    self._rlist.append(fileno)
            elif old_read:
                self._rlist.remove(fileno)
            if write:
                if not old_write:
                    self._wlist.append(fileno)
            elif old_write:
                self._wlist.remove(fileno)

        files_map[fileno] = event

    def unwatch(self, fileobj):
        super().unwatch(fileobj)
        if hasattr(fileobj, 'fileno'):
            fileno = fileobj.fileno()
        else:
            fileno = fileobj

        _, read, write = self._files_map.pop(fileno)
        if read:
            self._rlist.remove(fileno)
        if write:
            self._wlist.remove(fileno)

    def wait(self, timeout=None):
        super().wait(timeout)
        files_map = self._files_map
        if not files_map:
            raise ValueError('no file descriptors registered')

        # Call select().
        rlist, wlist, _ = select.select(self._rlist, self._wlist, (), timeout)

        # Look first in rlist to build the result.
        result = []
        for fileno in rlist:
            fileobj = files_map[fileno][0]
            # Check whether this file descriptor is also in wlist.
            try:
                wlist.remove(fileno)
            except ValueError:
                result.append(IOEvent(fileobj, True, False))
            else:
                result.append(IOEvent(fileobj, True, True))

        # Look for the remaining file descriptors in wlist.
        for fileno in wlist:
            fileobj = files_map[fileno][0]
            result.append(IOEvent(fileobj, False, True))

        return result

    if hasattr(dict, 'itervalues'):
        # Python 2.x
        def get_watched(self):
            super().get_watched()
            return self._files_map.values()
    else:
        # Python 3.x
        def get_watched(self):
            super().get_watched()
            return list(self._files_map.values())

    def clear(self):
        super().clear()
        del self._rlist[:]
        del self._wlist[:]
        self._files_map.clear()

    def close(self):
        super().close()
        try:
            del self._files_map
        except AttributeError:
            pass


class BasePollIOWait(AbstractIOWait):

    def __init__(self):
        self._files_map = {}

    def watch(self, *args, **kwargs):
        super().watch(*args, **kwargs)
        event = IOEvent._from_watch_args(*args, **kwargs)
        fileobj, read, write = event

        event_mask = self._read_mask if read else 0
        if write:
            event_mask |= self._write_mask

        if hasattr(fileobj, 'fileno'):
            fileno = fileobj.fileno()
        else:
            fileno = fileobj

        poll = self._poll
        try:
            poll.modify(fileno, event_mask)
        except IOError:
            poll.register(fileno, event_mask)

        self._files_map[fileno] = event

    def unwatch(self, fileobj):
        super().unwatch(fileobj)
        if hasattr(fileobj, 'fileno'):
            fileno = fileobj.fileno()
        else:
            fileno = fileobj

        del self._files_map[fileno]
        self._poll.unregister(fileno)

    def wait(self, timeout=None):
        super().wait(timeout)
        files_map = self._files_map
        if not files_map:
            raise ValueError('no file descriptors registered')

        # Call poll().
        timeout = timeout if timeout is not None else -1
        poll_result = self._do_poll(timeout)

        # Build and return the result.
        read_mask = self._read_mask
        write_mask = self._write_mask
        return [
            IOEvent(files_map[fileno][0],
                    bool(event & read_mask),
                    bool(event & write_mask))
            for fileno, event in poll_result]

    if hasattr(dict, 'itervalues'):
        # Python 2.x
        def get_watched(self):
            super().get_watched()
            return self._files_map.values()
    else:
        # Python 3.x
        def get_watched(self):
            super().get_watched()
            return list(self._files_map.values())

    def clear(self):
        super().clear()
        files_map = self._files_map
        for fileno in files_map:
            self._poll.unregister(fileno)
        files_map.clear()

    def close(self):
        super().close()
        try:
            del self._poll
            del self._files_map
        except AttributeError:
            pass


class PollIOWait(BasePollIOWait):
    """Implementation based on the poll() system function.

    Not available on all platforms.
    """

    available = hasattr(select, 'poll')

    def __init__(self):
        super().__init__()
        self._poll = select.poll()
        self._read_mask = select.POLLIN | select.POLLPRI
        self._write_mask = select.POLLOUT

    def _do_poll(self, timeout):
        if timeout < 0:
            return self._poll.poll(-1)
        else:
            return self._poll.poll(timeout * 1000)


class EPollIOWait(BasePollIOWait):
    """Implementation based on the epoll() system function.

    Only supported on Linux 2.5.44 and newer.
    """

    available = hasattr(select, 'epoll')

    def __init__(self):
        super().__init__()
        self._poll = select.epoll()
        self._read_mask = select.EPOLLIN | select.EPOLLPRI
        self._write_mask = select.EPOLLOUT

    def _do_poll(self, timeout):
        return self._poll.poll(timeout)


class KQueueIOWait(AbstractIOWait):

    available = hasattr(select, 'kqueue')

    def __init__(self):
        self._files_map = {}
        self._kqueue = select.kqueue()

    def watch(self, *args, **kwargs):
        super().watch(*args, **kwargs)
        event = IOEvent._from_watch_args(*args, **kwargs)
        fileobj, read, write = event

        if hasattr(fileobj, 'fileno'):
            fileno = fileobj.fileno()
        else:
            fileno = fileobj

        files_map = self._files_map
        try:
            del files_map[fileno]
        except KeyError:
            pass

        kevents = []
        if read:
            kevents.append(select.kevent(
                fileno, select.KQ_FILTER_READ,
                select.KQ_EV_ADD | select.KQ_EV_ONESHOT))
        if write:
            kevents.append(select.kevent(
                fileno, select.KQ_FILTER_WRITE,
                select.KQ_EV_ADD | select.KQ_EV_ONESHOT))

        files_map[fileno] = (fileobj, read, write, kevents)

    def unwatch(self, fileobj):
        super().unwatch(fileobj)
        if hasattr(fileobj, 'fileno'):
            fileno = fileobj.fileno()
        else:
            fileno = fileobj
        del self._files_map[fileno]

    def wait(self, timeout=None):
        super().wait(timeout)
        files_map = self._files_map
        if not files_map:
            raise ValueError('no file descriptors registered')

        # Build the changelist for the kqueue object.
        changelist = []
        for _fileobj, _, _, kevents in files_map.values():
            changelist.extend(kevents)

        # Call kqueue() and destroy the changelist.
        kqueue_result = self._kqueue.control(
            changelist, len(changelist) * 2, timeout)
        del changelist

        # Merge the kevents that share the same ident.
        kevents_merged = {}
        KQ_FILTER_READ = select.KQ_FILTER_READ
        KQ_FILTER_WRITE = select.KQ_FILTER_WRITE

        for kevent in kqueue_result:
            value = kevents_merged.setdefault(kevent.ident, [False, False])
            if kevent.filter == KQ_FILTER_READ:
                value[0] = True
            elif kevent.filter == KQ_FILTER_WRITE:
                value[1] = True
        del kqueue_result

        # Build and return the result.
        result = []
        for fileno, (read, write) in kevents_merged.items():
            try:
                activef = files_map[fileno]
            except KeyError:
                pass
            else:
                result.append(IOEvent(activef[0], read=read, write=write))
        return result

    def get_watched(self):
        super().get_watched()
        return [IOEvent(*item[:3]) for item in self._files_map.values()]

    def clear(self):
        super().clear()
        self._files_map.clear()

    def close(self):
        super().close()
        try:
            self._kqueue.close()
            del self._kqueue
            del self._files_map
        except AttributeError:
            pass


# Define IOWait.
if EPollIOWait.available:
    IOWait = EPollIOWait
elif KQueueIOWait.available:
    IOWait = KQueueIOWait
elif PollIOWait.available:
    IOWait = PollIOWait
else:
    IOWait = SelectIOWait
