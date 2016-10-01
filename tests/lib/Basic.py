import subprocess


class Basic():
    def __init__(self):
        self.file_out = open('out.txt', 'w')
        self.file_err = open('err.txt', 'w')

    def run_web_app(self):
        self.pid = subprocess.Popen('./webApp.R', stdout=self.file_out,
                                    stderr=self.file_err)
        print('run')

    def stop_web_app(self):
        self.pid.kill()
        self.file_out.close()
        self.file_err.close()
        print('stop')

    def download_file(self, url, filename):
        if url is None or url == '':
            raise Exception('No url')
        cmd = 'wget {0} -O {1}'.format(url, filename)
        subprocess.call(cmd, shell=True)
