# Hit

Almost everyone I know stores their git repositories in a single folder on their local machine.

`hit` is a multi-repo-agnostic version of `git`, which will use its working directory to figure out how many git
repositories are directly under it. Invoke it like you would `git`, and it will invoke the correct version of `git`
under the hood. **THIS MEANS** it can do things like:

* Ensuring all your branches have the same name: `hit checkout -b <my-branch>`
* Listing files: `hit ls-files`

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

# Future improvements

Currently, `hit` assumes that you always want to target all subdirectories. Soon there will be a way to differentiate a
single or group of subdirectories.
