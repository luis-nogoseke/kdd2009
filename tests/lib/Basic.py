import subprocess


class Basic():
    def __init__(self):
        self.file_out = open('out.txt', 'w')
        self.file_err = open('err.txt', 'w')
        print('init')

    def run_web_app(self):
        self.pid = subprocess.Popen('./webApp.R', stdout=self.file_out,
                                    stderr=self.file_err)
        print('run')

    def stop_web_app(self):
        self.pid.kill()
        self.file_out.close()
        self.file_err.close()
        print('stop')
