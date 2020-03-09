# Hit

Almost everyone I know stores their git repositories in a single folder on their local machine.

`hit` is a multi-repo-agnostic version of `git`, which will use its working directory to figure out how many git
repositories are directly under it. Invoke it like you would `git`, and it will invoke the correct version of `git`
under the hood. This means it can do things like:

* Ensuring all your branches have the same name: `hit checkout -b <my-branch>`
* Listing files: `hit ls-files`
* Checking the status of a few repositories in a project: `hit backend status`

# Quick start

Clone the repo, install the binary with `stack` (https://docs.haskellstack.org/en/stable/README/), and make sure
`~/.local/bin` is in your `$PATH`.

```bash
curl -sSL https://get.haskellstack.org/ | sh
git clone https://github.com/dfithian/hit.git
cd hit
stack install
hit ls-files
```

Then create a `~/.hitconfig` file with the following schema:

```yaml
- name: fullstack
  home: /Users/<user>/work/git # **NOTE** `~` doesn't work yet
  dirs:
    - frontend-repo
    - backend-repo
```

Now you can use `hit`:

```bash
hit fullstack status
```

Or, if you're in the `~/work/git` directory, you can get info on all repositories:

```bash
hit status
```

# Supported commands

* `ls-files`
* `status`
* `diff`

Other commands may work, like `checkout`, `branch`, `commit`, `pull`, and `push`, but they are not tested.

# Disclaimers and future improvements

* `~` expansion doesn't work yet, so you must specify a full path for the `home` key
