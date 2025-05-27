# KnitSpeak
This is an implementation of the KnitSpeak knitting pattern language. Based on the [specification](https://stitch-maps.com/about/knitspeak/) from [Stitch Maps](https://stitch-maps.com).

To install this implementation clone the repository. Then run 
```
stack install
```

To just format a pattern you can call: 
```
knitSpeak -- <PATTERN>.ks
```
To write the formated pattern to file:
```
knitSpeak -- <PATTERN>.ks <OUTPUT-FILE>
```

## Operations

The implementation currently supports the changing patterns with Mirroring, Inverting, Flipping, Unrolling and Minimizing the patterns. 

To run these you can call: 
``` 
knitSpeak -- -f <PATTERN>.ks
knitSpeak -- -s <PATTERN>.ks
knitSpeak -- -i <PATTERN>.ks
knitSpeak -- -m <PATTERN>.ks
knitSpeak -- -u <PATTERN>.ks
knitSpeak -- -ui <PATTERN>.ks
``` 
`ui` unrolls all fixed length compressed instructions, while `u` unrolls compressed lines in a  pattern. 

In addition we can assert that the parser is idempotent by calling:
```
knitSpeak -- -p <PATTERN>.ks
```

To see all possible flags you can call:
```
knitSpeak
```
, or:
```
knitSpeak -- -h
```



## TODO
- [ ] Allow all operations to write resulting pattern to file
- [ ] Unroll multiline repeats in `unrollLines` function
