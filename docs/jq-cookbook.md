# jq cookbook
https://stedolan.github.io/jq/manual/#ConditionalsandComparisons

https://github.com/stedolan/jq/wiki/Cookbook#filter-objects-based-on-the-contents-of-a-key

### select()
```
$ json='[{"genre":"deep house"}, {"genre": "progressive house"}, {"genre": "dubstep"}]'
$ echo "$json" | jq -c '.[] | select(.genre | contains("house"))'
{"genre":"deep house"}
{"genre":"progressive house"}
```

### variables holding value from parent object:
```
jq -r '.[] as $identity | $identity.roles[] as $role | [$role.id,$role.name,$identity.id,$identity.email,$identity.fullName] | @csv' < identities.txt | sort -n
```

```
jq '.[]|.date as $date|.notes[]|[$date,.category,.note]|@csv' < releaseNotes.json
```

### keys[]
```
jq -r 'keys[] as $k | "\($k), \(.[$k] | .ip)"'
```

### to_entries[]
```
jq -r 'to_entries[] | "\(.key), \(.value | .ip)"'
```
