These files are meant to be used in GTKWave to make enum decoding easier.

In GTKWave, add a signal and set it to binary. 

```
Right click -> Data format -> Binary
```

Then, select and enable the filter translation file.

 ```
 Right click -> Data format -> Translate filter file -> Enable and Select -> Add filter to list -> (select a file) -> (make sure the file is highlighted) -> OK
 ```
 
 GTKWave should now use the enum decoding values in the file instead of the raw binary values. 