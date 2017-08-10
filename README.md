# yt-utilities

## Export org-mode to YT


Example:
```
./Export.hs --user georgeee --token perm:Z2VvcmdlZWU=.VGVzdDE=.nPgExkZGKX4qNkr2RxdHHlmVNRxeKu --org test.org --since 2017-08-05
./Export.hs --user georgeee --slack-token xoxp-9111111113-52222222226-220000000008-4aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4 --token perm:Z2VvcmdlZWU=.VGVzdDE=.nPgExkZGKX4qNkr2RxdHHlmVNRxeKu --org test.org --since 2017-08-05 --mode Export
```

If no `--slack-token` param supplied, message to Slack won't be sent.
To get Slack token, visit https://api.slack.com/custom-integrations/legacy-tokens.

With `--mode=<mode>` you may specify mode to run, following are supported:
* `Dry` (default) -- dry run (read-only sync with YT)
* `Export` -- export to YT
* `Local` -- print table of local file (no sync with YT at all)
* `Local2YT` -- print table for local file, in format for Slack message

Org-mode file is analyzed for CLOCK and TRACK records, only trees with `ytexport` string in header name (or org tag) are analyzed.
* CLOCK items are interpreted as expected. Notice: only closed intervals are considered valid, half-closed are ignored.
* TRACK items are custom format for recording interval tracks like "TRACK 2017-09-11 1:12" (1h12m spent on 9th September).

See `test.org` as example of org file. For more information, check `./Export.hs --help`.

To get token:
1. Open https://issues.serokell.io/users/me
2. Click 'Update personal information and manage logins' link
3. Open Authentication tab
4. New token (enter name, scope: Youtrack)
