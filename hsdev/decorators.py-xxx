

import SublimeHaskell.hsdev.callback as HsCallback

def hsdev_command(async=False, timeout=None, is_list=False):
    def wrap_function(cmd_fn):
        def wrapped(self, *args, **kwargs):
            wait_flag = kwargs.pop('wait', not async)
            timeout_arg = kwargs.pop('timeout', timeout)
            on_resp = kwargs.pop('on_response', None)
            on_not = kwargs.pop('on_notify', None)
            on_err = kwargs.pop('on_error', None)
            on_res_part = kwargs.pop('on_result_part', None)
            split_res = kwargs.pop('split_result', on_res_part is not None)

            (name_, opts_, on_result_) = cmd_fn(self, *args, **kwargs)

            if is_list and split_res:
                result = []

                def on_notify(reply):
                    if 'result-part' in reply:
                        notify_result = on_result_([reply['result-part']])[0]
                        HsCallback.call_callback(on_res_part, notify_result)
                        result.append(notify_result)
                    else:
                        HsCallback.call_callback(on_not, reply)

                def on_response(_):
                    on_resp(result)

                opts_.update({'split-result': None})  # FIXME: Is this option still used?
                resp = self.call(name_
                                 , opts_
                                 , on_response=on_response if on_resp else None
                                 , on_notify=on_notify
                                 , on_error=on_err
                                 , wait=wait_flag
                                 , timeout=timeout_arg)

                return result if wait_flag else resp

            else:
                def on_response(resp):
                    on_resp(on_result_(resp))

                resp = self.call(name_
                                 , opts_
                                 , on_response=on_response if on_resp else None
                                 , on_notify=on_not
                                 , on_error=on_err
                                 , wait=wait_flag
                                 , timeout=timeout_arg)
                return on_result_(resp) if wait_flag else resp

        return wrapped
    return wrap_function


def command(cmd_fn):
    return hsdev_command(async=False, timeout=1)(cmd_fn)


def async_command(async_fn):
    return hsdev_command(async=True)(async_fn)


def list_command(list_fn):
    return hsdev_command(async=False, timeout=1, is_list=True)(list_fn)


def async_list_command(list_fn):
    return hsdev_command(async=True, is_list=True)(list_fn)
