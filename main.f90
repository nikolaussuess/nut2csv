! This program parses the nuttcp output.
! It reads the input from STDIN s.t. the input can be piped and prints the
! output (as CSV) to STDOUT, if no other behavior is specified. Read "--help" for more infos.
! Logging-messages are printed to STDERR.
program NUT2CSV
    use f90getopt
    use, intrinsic :: iso_fortran_env, only : STDERR => error_unit,  &
                                              STDOUT => output_unit, &
                                              STDIN  => input_unit
    implicit none

    character(*), parameter:: VERSION = "0.0.2"             ! Version number

    ! Flags
    logical                :: print_nuttcp_verbose          ! if the verbose messages of nuttcp should be printed to stderr
    logical                :: parse_brief_only              ! parse last line (brief output), only!
    logical                :: always_retrans                ! Default print retrans only if it is also in the input format;
                                                            ! whith this flag you can print it always

    logical                :: has_error                     ! True if at least one parsing error occured, false otherwise

    ! command line arguments
    type(option_s), parameter :: opts(*) = [ &
                    option_s("brief", .false., "b"), &
                    option_s("always-retrans", .false., "r"), &
                    option_s("verbose", .false., "v"), &
                    option_s("help", .false., "h"), &
                    option_s("version", .false., "V"), &
                    option_s("input-file", .true., "i"), &
                    option_s("output-file", .true., "o") &
                ]

    ! Variables
    integer                :: stat                          ! IO status
    character(len=1024)    :: line                          ! Full input line; input is read line by line
    integer                :: line_number                   ! Line number, displayed along with errors

    real(kind=16)          :: mbps, interval, bandwidth     ! Numbers to be printed
    integer                :: retrans                       ! Optional integer with retransmittion
    real(kind=16)          :: time_sum                      ! Sums the (interval) time up

    character(len=1024)    :: help1, help2, help3, help4    ! helper variables; store the text between the numbers

    ! (additional) brief output variables
    integer                :: tx, rx
    real(kind=16)          :: msrtt

    ! Files
    integer                :: input_file, output_file       ! file descriptors of input and output file

    ! LABELS
    !  99 ... EOF (End of file / End of input)
    !  80 ... input format of the nuttcp line
    !  81 ... output format for file
    !  83 ... input format of brief line
    !  84 ... output format of brief line

    ! Initialize the time
    time_sum = 0
    retrans = 0
    line = ""            ! Also initialize the line, so that in case of an empty input/error the behavior is defined
    input_file = STDIN   ! read from STDIN if no input file is specified
    output_file = STDOUT ! write to STDOUT if no output file is specified

    ! Command line arguments
    parse_brief_only = .false.
    always_retrans = .false.
    print_nuttcp_verbose = .false.

    do
        select case(getopt("brvVhi:o:", opts))
            case(char(0)) ! all options processed
                exit
            case("b")
                parse_brief_only = .true.
            case("r")
                always_retrans = .true.
            case("v")
                print_nuttcp_verbose = .true.
            case("i")
                input_file = 90
                open(unit=input_file, file=trim(optarg), iostat=stat, status='old')
                if( stat /= 0 ) then
                    write(STDERR, *) "Error opening file '", trim(optarg), "' for input."
                    stop 2
                end if
            case("o")
                output_file = 91
                open(unit=output_file, file=trim(optarg), iostat=stat)
                if( stat /= 0 ) then
                    write(STDERR, *) "Error opening file '", trim(optarg), "' for output."
                    stop 2
                end if
            case("V")
                call print_version()
                stop
            case("h")
                call help()
                stop
        end select
    end do


    line_number = 0      ! set input line number to 0
    has_error = .false.  ! There was no error, yet ...

    ! Read line by line
    do
        read (input_file, '(1024A)', end=99) line
        line_number = line_number + 1

        ! ignore lines starting with "nuttcp" when parsing
        if( line(1:6) ==  "nuttcp" ) then
            if( print_nuttcp_verbose ) then
                write(STDERR, '(1024A)') line
            end if
            cycle
        end if

        ! if only the last line should be parsed, skip everything here ...
        if ( parse_brief_only ) cycle

        ! Ignore empty lines and do not report an error
        if ( TRIM(line) == "" ) cycle

        ! Parse the respective line
        ! The first fields are always the same, so we can here just parse them using read
        ! help1 ... "Mbps /"
        ! help2 ... "sec ="
        ! help3 ... "Mbps"
        ! help4 ... "retrans", but optional!
        read(UNIT=line, FMT=80, IOSTAT=stat) mbps, help1, interval, help2, bandwidth, help3, retrans, help4
        ! Check for correct input
        if ( stat /= 0 ) then
            ! If the line contains a %, it is (most likely) the last line which should be skipped
            ! So, this is not an error.
            if ( INDEX( line, "%" ) == 0 ) then
                write(STDERR,*) "%0 Wrong line format. Skipping line ", line_number,": "
                write(STDERR,*) trim(line)
                has_error = .true.
            end if
        else if ( ADJUSTL(TRIM(help1)) /= "MB /" ) then
            write(STDERR,*) "%1 Wrong line format. Skipping line ", line_number,": "
            write(STDERR,*) trim(line)
            has_error = .true.
        else if ( ADJUSTL(TRIM(help2)) /= "sec =" ) then
            write(STDERR,*) "%2 Wrong line format. Skipping line ", line_number,": "
            write(STDERR,*) trim(line)
            has_error = .true.
        else if ( ADJUSTL(TRIM(help3)) /= "Mbps" ) then
            write(STDERR,*) "%3 Wrong line format. Skipping line ",line_number,": ",help3
            write(STDERR,*) trim(line)
            has_error = .true.
        else if( ADJUSTL(TRIM(help4)) /= "retrans" .and. TRIM(help4) /= "" ) then
            write(STDERR,*) "%4 Line end not understood at line ", line_number,":"
            write(STDERR,*) trim(line)
            has_error = .true.
        else
            ! Output the CSV line to STDOUT
            time_sum = time_sum + interval
            write(output_file, 81, advance='no') time_sum,",",mbps, ",",interval,",", bandwidth
            ! Print retrans only if it is also in the input format, except if the option "always_retrans" is anabled
            if( ADJUSTL(TRIM(help4)) == "retrans" .or. always_retrans ) then
                write (output_file, '(A,I5)') ",", retrans
            end if
        endif
    end do

    ! EOF
    99 continue
        if( parse_brief_only ) then
            read(UNIT=line, FMT=83, IOSTAT=stat) mbps, help1, interval, help2, bandwidth, help3,&
                    tx,help4,rx,help1,retrans,help2,msrtt,help3
            if( stat /= 0 ) then
                write(STDERR, *) "%5 Error parsing brief line (line number ",line_number,")."
                has_error = .true.
            end if
            write(output_file, 84) mbps, ",", interval, ",", bandwidth, ",", tx, ",", rx, ",", retrans, ",", msrtt
        end if

        ! close files if open
        if( input_file /= STDIN ) close(unit=input_file)
        if( output_file /= STDOUT ) close(unit=output_file)

        write(STDERR,*) "EOF"

        if (has_error) then
            stop 1
        end if

    ! FORMAT STRINGS
    ! Read line format
    ! Example:
    !    94.1875 MB /   1.00 sec =  790.1107 Mbps     0 retrans
    80 FORMAT(1X, (F10.5,A5,F6.2,A6,F11.5,A5,I6,A8))
    ! Output format
    ! Example:
    !   1.00000,  94.18750,   1.00000, 790.11070, 0
    81 FORMAT(1X, (F10.5,A,F10.5,A,F11.5,A,F11.5))
    ! input format of last line
    ! Example:
    ! 29955.5000 MB /  10.00 sec = 25128.5977 Mbps 99 %TX 69 %RX 0 retrans 0.09 msRTT
    83 FORMAT(1X, (F10.5,A5,F6.2,A6,F10.5,A5,I3,A4,I3,A4,I2,A8,F5.2,A5))
    ! Output format
    ! Example:
    ! 29955.5000, 10.00, 25128.5977, 99, 69, 0, 0.09
    84 FORMAT(1X, (F10.5,A,F6.2,A,F10.5,A,I3,A,I3,A,I3,A,F10.5))


contains
    ! Prints the version number of nut2csv
    ! Used for --version / -V command line arg.
    subroutine print_version()
        print *, "NUT2CSV version ", VERSION, ", (c) 2020-2022 by Nikolaus Suess"
    end subroutine print_version

    ! Prints the help text
    subroutine help()
        character(len=1024) :: program_name

        ! get program name
        call get_command_argument(0, program_name)

        print *, "===== NUT2CSV ===== Version ", VERSION
        print *, "Converts the output of nuttcp to the CSV format, so that it can easily be processed in scripts."
        print *, ""
        print *, "CALL:"
        print *, "     ", trim(program_name)
        print *, "          Read from STDIN, write to STDOUT."
        print *, "     ", trim(program_name), " [OPTIONS]"
        print *, "          You can combine any of the options below."
        print *, ""
        print *, "OPTIONS:"
        print *, "-h, --help"
        print *, "          Prints this help."
        print *, "-b, --brief"
        print *, "          Only parse the last line, which is the brief output of nuttcp."
        print *, "-r, --always-retrans"
        print *, "          Always print the retrans value, even if it is not in the original output format."
        print *, "-v, --verbose"
        print *, "          Also include nuttcp debugging lines in the output."
        print *, "          That are lines starting with nuttcp-r or nuttcp-t."
        print *, "-i <FILENAME>, --input-file=<FILENAME>"
        print *, "          The file that should be converted."
        print *, "          If this parameter is not specified, the input is read from STDIN."
        print *, "-o <FILENAME>, --output-file=<FILENAME>"
        print *, "          Path to the output file."
        print *, "          If this parameter is not specified, the output is written to STDOUT."
        print *, ""
        print *, "OUTPUT FORMAT:"
        print *, "[Seconds since start], [Data in MB], [Time interval in seconds], [Throughput in Mbps], [retrans]"
        print *, ""
        print *, "Example:"
        print *, "   94.7500 MB /   1.00 sec =  794.7960 Mbps     0 retrans"
        print *, "   94.8750 MB /   1.00 sec =  795.9002 Mbps     0 retrans"
        print *, "Ouptput:"
        print *, "    1.00000,  94.75000,    1.00000,  794.79600,    0"
        print *, "    2.00000,  94.87500,    1.00000,  795.90020,    0"
        print *, ""
        print *, "ERROR CODES:"
        print *, "%0 - any unknown error, reported by IOSTAT parameter. E.g., formatting error, IO, ..."
        print *, "%1 - the first field does not end with 'MB /'."
        print *, "%2 - the second field does not end with 'sec ='."
        print *, "%3 - the third field does not end with 'Mbps'."
        print *, "%4 - the end of the line is not understood. It should be either 'retrans' or empty."
        print *, "%5 - any trouble with the brief line (i.e., the last line)."
        print *, "If possible, the program continues but returns an error code >0 at the end."
        print *, ""
        print *, "RETURN VALUES:"
        print *, "- 0 on success, when there was no error at all"
        print *, "- 1 if there was at least one parsing error"
        print *, "- 2 if the input or output file could not be opened."
        print *, ""
        print *, "(c) 2020-2022 by Nikolaus Suess <nikolaus.suess@univie.ac.at>"
        print *, "The code and program is published under GNU GPLv3."
    end subroutine help
end program NUT2CSV
