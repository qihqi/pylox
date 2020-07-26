import sys

had_error = False

def error(line, message):
    report(line, '', message)


def report(line, where,  message):
    print('[line {}] Error {}: {}'.format(line, where, message),
          file=sys.stderr)
    sys.stderr.flush()
    global had_error
    had_error = True

