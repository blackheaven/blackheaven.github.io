+++
title = "Zsh: debugging git remotes completion"
date = 2023-05-31
draft = false
path = "2023-05/zsh-git-debug"

[taxonomies]
categories = ["dev"]
tags = ["dev", "development", "flow", "tools", "zsh", "git"]
+++

I spend most of my day in a linux terminal ([KDE's konsole](https://apps.kde.org/fr/konsole/)), doing so, I interact directly with [git](https://git-scm.com/).

I usually starts working on a topic:

```sh
$ git co -b feature/something-useful
# 'co' is an alias for checkout
# I create a new branch
```

Then, I work intensively and I conclude by adding my changes in a commit

```sh
$ git add -p
# I stage each chuncks
$ git ci -m "feat(area): something useful (#42)"
```

Finally, I want to push by branch:

```sh
$ git push <Tab>
```

I try to trigger [zsh](https://www.zsh.org/)'s git completion, but after several seconds, I've got an unfortunate:

```sh
zsh: do you wish to see all 148622 possibilities (74311 lines)?
```

Going deeper, it looks like NSFW domains I have populated in my `/etc/hosts` with blocked hosts from [StevenBlack's hosts](https://github.com/StevenBlack/hosts/tree/master).

So, it breaks my flow, and does not give me relevant results.

In order to fix that, I have to limit completion to actual registered remotes.

First, I had to locate the completion function:

```sh
$ which _git
_git () {
        # undefined
        builtin autoload -XUz
}
```

Mmh, it does not look good, I had to pull, well, to fetch the tarball, the relevant functions are the following:

```sh
# zsh-5.9/Completion/Unix/Command/_git
(( $+functions[__git_remotes] )) ||
__git_remotes () {
  local remotes expl

  remotes=(${(f)"$(_call_program remotes git remote 2>/dev/null)"})
  __git_command_successful $pipestatus || return 1

  _wanted remotes expl remote compadd "$@" -a - remotes
}

(( $+functions[__git_remote_repositories] )) ||
__git_remote_repositories () {
  if compset -P '*:'; then
    _remote_files -/ -- ssh
  else
    _ssh_hosts -S:
  fi
}

(( $+functions[__git_local_repositories] )) ||
__git_local_repositories () {
  local expl

  _wanted local-repositories expl 'local repository' _directories
}

(( $+functions[__git_any_repositories] )) ||
__git_any_repositories () {
  # TODO: should also be $GIT_DIR/remotes/origin
  _alternative \
    'local-repositories::__git_local_repositories' \
    'remotes: :__git_remotes' \
    'remote-repositories::__git_remote_repositories'
}
```

The entry-point is `__git_any_repositories` which relies (`_alternative`) on three functions:
* `__git_local_repositories` which looks-up for my folders
* `__git_remotes` which relies on `git remote`, what I want
* `__git_remote_repositories` which actually pulls my `/etc/hosts`

Thankfully with the syntax:

```sh
(( $+functions[__git_fn] )) ||
__git_fn () {
}
```

the function is only defined when it wasn't defined previously, such as in my `~/.zshrc`:

```sh
__git_remote_repositories (){}
__git_local_repositories (){}
```

which grive me the expected behavior:

```sh
$ git push <Tab>
origin  upstream
```
