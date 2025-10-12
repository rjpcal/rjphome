# Git Cookbook

### push a specific/earlier commit

https://stackoverflow.com/questions/3230074/how-can-i-push-a-specific-commit-to-a-remote-and-not-previous-commits
```
git push <remotename> <commit SHA>:<remotebranchname>
```
typically:
```
git push origin <commit SHA>:master
```

### change remote:
```
git remote set-url origin ssh://git@github.com/OpenPathSec/hapi-auth-jwt2
```

### reset date of a git commit:
```
git commit --amend --reset-author --no-edit    # affects just the latest commit
git rebase --ignore-date         # affects all commit since upstream
```

### split recent commit into multiple:
```
git reset HEAD~
git add --patch <filename>       # type “?” for help
# OR:
git add -A --patch         # to interactively stage all changed files, including additions and deletions
git commit    # as usual
```

### generate csv history for auditors:
```
git log --since=2021-01-01 --format='%h%x00%ae%x00%aI%x00%s' src | perl -pe 'chomp; $_ = join(",", map { s/"/""/g; "\"$_\"" } split /\0/) . "\n"'
```

### prune remote branches:
```
git fetch origin --prune
```
or if origin is the only upstream, then just
```
git fetch --prune
```

### update multiple branches during a rebase:
https://medium.com/@tigerasks/rebase-once-1642b7dc0563
```
git fetch
git branch -f master origin/master
git rebase -i master --update-refs
git push +:
```

```
git config --global --add --bool rebase.updateRefs true
```
