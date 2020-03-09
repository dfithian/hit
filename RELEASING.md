# Releasing

1. Update `package.yaml` version

## Homebrew

1. Tar the binary: `stack build && cp $(stack path --local-install-root)/bin/hit . && tar cvf hit.tar hit`
1. Upload the tar file under the new version: https://bintray.com/dfithian/hit/hit
1. Update the version in homebrew: `cd $(brew --repo dfithian/dfithian) && vi Formula/hit.rb`
1. Commit and push to git
