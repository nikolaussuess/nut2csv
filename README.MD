# `nut2csv` &ndash; `nuttcp` output to CSV converter
This program converts the output of `nuttcp` to the CSV format, so that it can easily be processed in scripts.

[`nuttcp`](https://www.nuttcp.net) is a network performance measurement tool (similar to `iperf3`) that can, for example, measure the throughput in a network. Please find more information about the tool at [https://www.nuttcp.net](https://www.nuttcp.net). On Debian-based systems, you can istall it with:
```
apt install nuttcp
``` 

The output of `nuttcp` is easily human-readable; however, if you want to use it with a script, it is not so ideal. Our tool converts the output of `nuttcp` to CSV.

## Build
### Requirements
* Fortran compiler with (at least partial) Fortran 2008 support, like `gfortran`
* `f90getopt` from [https://github.com/haniibrahim/f90getopt](https://github.com/haniibrahim/f90getopt)
* (optional) `make` to run the `Makefile`

### Build
Clone the `f90getopt` from GitHub into the `./lib` folder:
```
mkdir -p lib
cd lib
git clone https://github.com/haniibrahim/f90getopt.git
cd ..
```

To start building, just run the `Makefile`:
```
make clean
make 
```

This will produce a (statically linked) executable `nut2csv`.

## Run
To execute the program, just run `./nut2csv`.

`--help` shows you the following help message:
```
 ===== NUT2CSV ===== Version 0.0.2
 Converts the output of nuttcp to the CSV format, so that it can easily be processed in scripts.
 
 CALL:
      ./nut2csv
           Read from STDIN, write to STDOUT.
      ./nut2csv [OPTIONS]
           You can combine any of the options below.
 
 OPTIONS:
 -h, --help
           Prints this help.
 -b, --brief
           Only parse the last line, which is the brief output of nuttcp.
 -r, --always-retrans
           Always print the retrans value, even if it is not in the original output format.
 -v, --verbose
           Also include nuttcp debugging lines in the output.
           That are lines starting with nuttcp-r or nuttcp-t.
 -i <FILENAME>, --input-file=<FILENAME>
           The file that should be converted.
           If this parameter is not specified, the input is read from STDIN.
 -o <FILENAME>, --output-file=<FILENAME>
           Path to the output file.
           If this parameter is not specified, the output is written to STDOUT.
 
 OUTPUT FORMAT:
 [Seconds since start], [Data in MB], [Time interval in seconds], [Throughput in Mbps], [retrans]
 
 Example:
    94.7500 MB /   1.00 sec =  794.7960 Mbps     0 retrans
    94.8750 MB /   1.00 sec =  795.9002 Mbps     0 retrans
 Ouptput:
     1.00000,  94.75000,    1.00000,  794.79600,    0
     2.00000,  94.87500,    1.00000,  795.90020,    0
 
 ERROR CODES:
 %0 - any unknown error, reported by IOSTAT parameter. E.g., formatting error, IO, ...
 %1 - the first field does not end with 'MB /'.
 %2 - the second field does not end with 'sec ='.
 %3 - the third field does not end with 'Mbps'.
 %4 - the end of the line is not understood. It should be either 'retrans' or empty.
 %5 - any trouble with the brief line (i.e., the last line).
 If possible, the program continues but returns an error code >0 at the end.
 
 RETURN VALUES:
 - 0 on success, when there was no error at all
 - 1 if there was at least one parsing error
 - 2 if the input or output file could not be opened.
 
 (c) 2020-2022 by Nikolaus Suess <nikolaus.suess@univie.ac.at>
 The code and program is published under GNU GPLv3.
```

## Examples
```bash
# Example 1
# Directly generate a CSV file.
nuttcp -T300 -i1 <IP> | nut2csv >output.csv

# Example 2
# Convert an existing *.txt file to *.csv
nuttcp -T300 -i1 <IP> >output.txt
nut2csv -i output.txt -o output.csv
```

## License
Copyright (C) 2022 by Nikolaus Suess

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

## Bug report
If you have any problems, please feel free to contact me at <[nikolaus.suess@univie.ac.at](mailto:nikolaus.suess@univie.ac.at)>.