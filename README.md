The input for the parser is a file with one or more MARC records (currently 5 MARC record types are supported: Marc21, Marc21 - Library of Congress, Marc with LEADER, UNIMARC, UNIMARC inline) of the same or different types, and the output that the parser produces are MARC structured records in JSON format.

In order to use the parse, you need to load marc_parser.hs and then run main with 2 CL arguments, namely the input and output file paths.
