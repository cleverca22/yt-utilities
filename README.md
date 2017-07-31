# yt-utilities

## Export org-mode to YT


Example:
```
./Export.hs --org test.org --user georgeee --token perm:Z2VvcmdlZWU=.VGVzdDE=.nPgExkZGKX4qNkr2RxdHHlmVNRxeKu 
```

Org-mode file is analyzed for CLOCK and TRACK records, only trees with `{yt_timetracking}` tag are analyzed.
* CLOCK items are interpreted as expected
* TRACK items are custom format for recording interval tracks like "TRACK 2017-09-11 1:12" (1h12m spent on 9th September)

See `test.org` as example of org file


To get token:
1. Open https://issues.serokell.io/users/me
2. Click 'Update personal information and manage logins' link
3. Open Authentication tab
4. New token (enter name, scope: Youtrack)
