### 2019-rac-parking

This is the *working* repository for compiling local authority parking reports for the UK (at the moment only Scotland and Wales).

Your best point of entry is the user's manual which can be found [here](https://github.com/majazaloznik/2019-rac-parking/blob/master/docs/technical/instructions/user-guide.pdf). 

I have intentionally removed the raw data for Scotland 2017/18, even though it is available (and has been tested). This means you can do a proper trial run of getting the new data, and running the update script to import it all, and produce a brand new report, just like the code is meant to be used in practice. 

You can also reproduce existing reports for Scotland and Wales for previous years (don't go too far back though..). 

The one main issue with the code as it stands is that I'm still not sure about the total transportation numbers or rather how the original reports treated them (or rather they seem to treat them inconsistently, while these reports are consistent). I have not been able to replicate that, so those numbers will be off. Everything else should match up (in all the cases that I've found that it doesn't, double checking has revealed an error in the original reports). 