# yt-utilities

## Export org-mode to YT


Example:
```
./Export.hs --org test.org --user georgeee --token perm:Z2VvcmdlZWU=.VGVzdDE=.nPgExkZGKX4qNkr2RxdHHlmVNRxeKu --since 2017-08-02
```

Org-mode file is analyzed for CLOCK and TRACK records, only trees with `ytexport` string in header name (or org tag) are analyzed.
* CLOCK items are interpreted as expected. Notice: only closed intervals are considered valid, half-closed are ignored.
* TRACK items are custom format for recording interval tracks like "TRACK 2017-09-11 1:12" (1h12m spent on 9th September).

See `test.org` as example of org file. For more information, check `./Export.hs --help`. 

To get token:
1. Open https://issues.serokell.io/users/me
2. Click 'Update personal information and manage logins' link
3. Open Authentication tab
4. New token (enter name, scope: Youtrack)
