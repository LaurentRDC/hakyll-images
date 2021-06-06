# Release checklist

[ ] Update CHANGELOG.md if necessary.
[ ] Tag release in git.
    `git tag -a "version" && git push origin "version"`
    A new GitHub release will automatically be generated
[ ] Upload to Hackage:
    `stack upload .`