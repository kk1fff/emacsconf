#!/usr/bin/env python

import os, tempfile, subprocess, select
import shutil, sys, urllib, tarfile, traceback

class InitElHandler:
    """
    Collect modifications that each package wants to make on init.el
    """
    def __init__(self):
        self._line_buffer = []

    def append_line(self, line):
        self._line_buffer.append(line)

    def get_all(self):
        return '\n'.join(self._line_buffer)

    """
    Accept a bunch of lines at one invocation
    """
    def append_lines(self, lines):
        for line in lines:
            self.append_line(line)

class Package:
    def __init__(self, name):
        self._name = name

    def install_package(self, target_prefix, init_el_handler):
        pass

    def get_name(self):
        return self._name

class LocalPackage(Package):
    def __init__(self):
        Package.__init__(self, 'local_package')

    def install_package(self, target_prefix, init_el_handler):
        shutil.copytree('local_packages', target_prefix + "/local_packages")
        init_el_handler.append_line(
            "(add-to-list 'load-path \"{}/local_packages\")".format(target_prefix))
        init_el_handler.append_line(
            "(add-to-list 'load-path \"{}/local_packages/emacs-nav-49\")".format(target_prefix))

class GitBasedPackage(Package):
    def __init__(self, name, repo):
        Package.__init__(self, name)
        self._repo = repo

    def install_package(self, target_prefix, init_el_handler):
        target = target_prefix + '/' + self._name

        self.pre_install(target, init_el_handler)

        tmp_dir = tempfile.mkdtemp(prefix='emacs')
        print "Getting: {} from {}".format(self._name, self._repo)
        try:
            repo_dir = tmp_dir + "/" + self._name
            self._do_git_clone(repo_dir)
            shutil.move(repo_dir, target)
        except Exception as e:
            raise e
        finally:
            shutil.rmtree(tmp_dir)
            tmp_dir = None

        self.post_install(target, init_el_handler)

    def _do_git_clone(self, repo_dir):
        gitproc = subprocess.Popen(['git', 'clone', self._repo,
                                    repo_dir],
                                   stdout = subprocess.PIPE,
                                   stderr = subprocess.PIPE)
        buf = ""
        while True:
            fd = select.select([gitproc.stdout.fileno(),
                                gitproc.stderr.fileno()],
                               [], [])
            for f in fd[0]:
                if f == gitproc.stdout.fileno():
                    buf = buf + gitproc.stdout.readline()
                if f == gitproc.stderr.fileno():
                    buf = buf + gitproc.stderr.readline()
            r = gitproc.poll()
            if r != None:
                if r != 0:
                    raise Exception(
                        "git exited with {}, output:\n {}".format(r, buf))
                break

    def pre_install(self, target, init_el_handler):
        pass

    def post_install(self, target, init_el_handler):
        pass

class HttpBasedPackage(Package):
    def __init__(self, name, url):
        Package.__init__(self, name)
        self._url = url

    def install_package(self, target_prefix, init_el_handler):
        target = target_prefix + '/' + self._name

        self.pre_install(target, init_el_handler)

        tmp_dir = tempfile.mkdtemp(prefix='emacs')
        print "Getting: {} from {}".format(self._name, self._url)
        try:
            repo_dir = tmp_dir + "/" + self._name
            self.handle_package(self._do_download(tmp_dir),
                                tmp_dir,
                                repo_dir)
            shutil.move(repo_dir, target)
        except Exception as e:
            traceback.print_exc()
            raise e
        finally:
            shutil.rmtree(tmp_dir)
            tmp_dir = None

        self.post_install(target, init_el_handler)

    def _do_download(self, tmp_dir):
        r = urllib.urlretrieve(self._url, tmp_dir + "/pkg")
        return r[0]

    def handle_package(self, package_file, tmp_dir, repo_dir):
        pass

    def pre_install(self, target, init_el_handler):
        pass

    def post_install(self, target, init_el_handler):
        pass

class TarGzHttpBasedPackage(HttpBasedPackage):
    def __init__(self, name, url):
        HttpBasedPackage.__init__(self, name, url)

    def handle_package(self, package_file, tmp_dir, repo_dir):
        f = tarfile.open(package_file, "r:gz")
        f.extractall(repo_dir)
        self.post_extract(repo_dir)
        f.close()

    def post_extract(self, repo_dir):
        pass

class GitPackageSimple(GitBasedPackage):
    def __init__(self, name, repo):
        GitBasedPackage.__init__(self, name, repo)

    def post_install(self, target, init_el_handler):
        init_el_handler.append_line(
            "(add-to-list 'load-path \"{}\")".format(target));

class GitThemePackage(GitBasedPackage):
    def __init__(self, name, repo):
        GitBasedPackage.__init__(self, name, repo)

    def post_install(self, target, init_el_handler):
        init_el_handler.append_line(
            "(add-to-list 'custom-theme-load-path \"{}\")".format(target));

class HttpTarGzSimplePackage(TarGzHttpBasedPackage):
    def __init__(self, name, url):
        TarGzHttpBasedPackage.__init__(self, name, url)

    def post_install(self, target, init_el_handler):
        init_el_handler.append_line(
            "(add-to-list 'load-path \"{}\")".format(target));

class MozillaCStyle(HttpTarGzSimplePackage):
    def __init__(self):
        HttpTarGzSimplePackage.__init__(self,
                                       "mozilla-c-style",
                                       "http://hg.mozilla.org/users/jblandy_mozilla.com/mozilla-elisp/archive/tip.tar.gz")
    def post_extract(self, repo_dir):
        tmp_dir = tempfile.mkdtemp()
        real_dir = repo_dir + "/" + os.listdir(repo_dir)[0]
        shutil.move(real_dir, tmp_dir + "/d")
        shutil.rmtree(repo_dir)
        shutil.move(tmp_dir + "/d", repo_dir)
        shutil.rmtree(tmp_dir)

class Installer:
    def __init__(self,
                 target_dir = os.environ['HOME'] + '/.emacs.d'):
        if os.uname()[0] == "Darwin":
            self._path_of_emacs = "/Applications/Emacs.app/Contents/MacOS/Emacs"
        else:
            self._path_of_emacs = "emacs"
        self._tmp_dir = tempfile.mkdtemp(prefix='emacs')
        self._target_dir = target_dir
        self._loaded_pkgs = None
        self._init_el_handler = InitElHandler()

    def reset_target_dir(self):
        if os.path.exists(self._target_dir):
            shutil.rmtree(self._target_dir)
        os.mkdir(self._target_dir)

    def install_package(self, pkgs):
        self._loaded_pkgs = []
        for pkg in pkgs:
            try:
                pkg.install_package(self._target_dir, self._init_el_handler)
                self._loaded_pkgs.append(pkg)
            except Exception as e:
                print "Failed to get {}, message: {}".format(pkg.get_name(),
                                                             str(e))
                traceback.print_exc()

    def install_init_el(self):
        init_el = open(self._target_dir + "/init.el", "w")
        src = open("init.el", "r")
        init_el.write(self._init_el_handler.get_all())
        init_el.write("\n")
        while True:
            l = src.readline()
            if l == "":
                break
            init_el.write(l)
        init_el.close()
        src.close()

    def run_install_lisp_script(self):
        print "Run installation script:"
        p = subprocess.Popen([self._path_of_emacs, '-q', '-l', 'install.el'],
                             stdout = subprocess.PIPE,
                             stderr = subprocess.PIPE)
        while True:
            fd = select.select([p.stdout.fileno(),
                                p.stderr.fileno()],
                               [], [])
            for f in fd[0]:
                if f == p.stdout.fileno():
                    sys.stdout.write(p.stdout.readline())
                if f == p.stderr.fileno():
                    sys.stderr.write(p.stderr.readline())
            r = p.poll()
            if r != None:
                if r != 0:
                    raise Exception("emacs exited with {}.".format(r))
                break


packages = [
    GitPackageSimple("helm",
                     "https://github.com/emacs-helm/helm.git"),
    GitPackageSimple("helm-gtags",
                     "git://github.com/syohex/emacs-helm-gtags.git"),
    GitPackageSimple("powerline",
                     "git://github.com/kk1fff/emacs-package-powerline.git"),
    GitPackageSimple("jade-mode",
                     "https://github.com/kk1fff/emacs-package-jade-mode.git"),
    GitPackageSimple("php-mode",
                     "https://github.com/kk1fff/emacs-package-php-mode.git"),
    GitPackageSimple("multi-web-mode",
                     "https://github.com/kk1fff/emacs-package-multi-web-mode.git"),
    MozillaCStyle(),
    LocalPackage(),
    GitThemePackage("emacs-theme",
                    "git://github.com/kk1fff/emacs-themes.git")
]

if __name__ == '__main__':
    installer = Installer()
    installer.reset_target_dir()
    installer.install_package(packages)
    installer.install_init_el()
    installer.run_install_lisp_script()
