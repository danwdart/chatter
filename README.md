# Chatter

## Purpose
Bots, but with a human component, normally.

Gives your personal details automatically when asked.

Logs into wandering chatrooms for you.

Interactive by prompt or by linkage.

## Networks
Jabber?
Omegle
Discord
Slack?
Your own SSH servers

## Interactivity
Jabber
Omegle
Discord
Console/readline
Telnet possibly - although that might as well be a user-based xinetd or nc / openssl

## Encryption supported
OTR
OpenSSL

## Structure

Separate executables in UNIX style for:

1. [x] rlwrap - Just a readline with custom prompt if console, maybe with logging on it
1. Web interface perhaps, to use as a From-Network
1. Networks to connect from, if not console (stdin = "network", stdout = text)
1. [x] Expect - Text replacement and automatic adding (text -> text) - just an `interact` script really
1. Networks to connect to (stdin = text, stdout = text from network)
1. Script to link all together, perhaps with a file to configure it.

Some of these may already exist for us.

## Configuration

See example files in config/