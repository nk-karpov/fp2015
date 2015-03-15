data LogLevel = Error | Warning | Info
cmp :: LogLevel -> LogLevel -> Ordering

cmp Error Error = EQ
cmp Error _ = GT
cmp _ Error = LT
cmp Warning Warning = EQ
cmp Warning _ = GT
cmp _ Warning = LT
cmp _ _ = EQ
