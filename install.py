#!/usr/bin/env python

class Installer:
    def create_temp_dir():
        pass

    def get_or_make_cache_dir():
        pass

    """
    If the package dir is not existed, we create it and initialize it as an empty
    git folder.
    """
    def get_or_make_package_dir():
        pass

    """
    Update current git repo.
    """
    def update_from_git(self, name, repo):
        pass

    def load_repo(self, repo):
        for pkg in repo:
            self.clone_or_update_from_git(pkg["name"], pkg["repo"])

    def install_local_package(self):
        

repo = [
    {
        "name": "helm",
        "repo": "git://github.com/kk1fff/emacs-package-powerline.git"
    },
    {
        "name": "emacs-theme",
        "repo": "git://github.com/kk1fff/emacs-themes.git"
    },
    {
        "name": "jade-mode",
        "repo": "https://github.com/kk1fff/emacs-package-jade-mode.git"
    },
    {
        "name": "php-mode",
        "repo": "https://github.com/kk1fff/emacs-package-php-mode.git"
    },
    {
        "name": "multi-web-mode",
        "repo": "https://github.com/kk1fff/emacs-package-multi-web-mode.git"
    }
]

if __name__ != '__main__':
    return

installer = Installer()
installer.load_repo(repo)
installer.install_local_package()
installer.run_install_elisp()
