# MARC parser

A parser for the MARC format for bibliographic data using the Parsec library. The MARC standard is used for describing the structure that is cataloged in libraries, for example books.

## Description

MARC is an abbreviation of Machine-Readable Cataloging and it represents a collection of different standards for representing the metadata of books, magazines etc. The metadata of a book is represented by a single bibliographic record which consists of a set of fields and subfields and a few additional elements. The records can come up in different representations. This parser allows parsing of multiple formats, and is meant to allow easy addition of new formats.

The input for the parser is a file with one or more MARC records (currently 5 MARC record types are supported: MARC21, MARC21 - Library of Congress, MARC with LEADER, UNIMARC, UNIMARC inline) of the same or different types, and the output that the parser produces are MARC structured records in JSON format. The JSON created follows the standards on this page: https://github.com/marc4j/marc4j/wiki/MARC-in-JSON-Description

## General structure of MARC records

In the most general case, a MARC record consists of three parts: the header (leader), a set of control fields and, finally, data fields. The leader usually has a fixed length of 24 characters. All fields (both control and data) have a tag comprised of three digits. Additionally, data fields have two indicators (two digits), and a list of subfields. Each subfield has its own tag, a digit or lowercase letter.

## Example

Input:
```
000 00888cam a2200253u 4500
001 6840872
005 20190127101238.0
007 cr_|||||||||||
008 820625s1917 ctuab 000 0 eng
906 __ |a 0 |b cbc |c premunv |d u |e ncip |f 19 |g y-gencatlg
010 __ |a 17025544
035 __ |9 (DLC) 17025544
040 __ |a DLC |c CarP |d DLC
050 00 |a PR2810.A2 |b H4
100 1_ |a Shakespeare, William, |d 1564-1616.
245 14 |a The first part of King Henry the Fourth,
260 __ |a New Haven, |b Yale university press; [etc., etc.] |c 1917.
300 __ |a 4 p. |b l., 148 p. illus. (map) |c 18 cm.
530 __ |a Also available in digital form.
600 00 |a Henry |b IV, |c King of England, |d 1367-1413 |x Drama.
700 1_ |a Hemingway, Samuel Burdett, |d 1883-1958, |e ed.
856 41 |u http://hdl.loc.gov/loc.gdc/scd0001.00139980359
991 __ |b c-GenColl |h PR2810.A2 |i H4 |t Copy 1 |w PREM
985 __ |a massdig |e sloan
```

Output:
```
[
	{
		"leader":"00888cam a2200253u 4500",
		"fields":
		[
			{
				"001":"6840872"
			},
			{
				"005":"20190127101238.0"
			},
			{
				"007":"cr_|||||||||||"
			},
			{
				"008":"820625s1917 ctuab 000 0 eng"
			},
			{
				"906":
				{
					"ind1":"_",
					"ind2":"_",
					"subfields":
					[
						{
							"a":"0"
						},
						{
							"b":"cbc"
						},
						{
							"c":"premunv"
						},
						{
							"d":"u"
						},
						{
							"e":"ncip"
						},
						{
							"f":"19"
						},
						{
							"g":"y-gencatlg"
						}
					]
				}
			},
			{
				"010":
				{
					"ind1":"_",
					"ind2":"_",
					"subfields":
					[
						{
							"a":"17025544"
						}
					]
				}
			},
			{
				"035":
				{
					"ind1":"_",
					"ind2":"_",
					"subfields":
					[
						{
							"9":"(DLC) 17025544"
						}
					]
				}
			},
			{
				"040":
				{
					"ind1":"_",
					"ind2":"_",
					"subfields":
					[
						{
							"a":"DLC"
						},
						{
							"c":"CarP"
						},
						{
							"d":"DLC"
						}
					]
				}
			},
			{
				"050":
				{
					"ind1":"0",
					"ind2":"0",
					"subfields":
					[
						{
							"a":"PR2810.A2"
						},
						{
							"b":"H4"
						}
					]
				}
			},
			{
				"100":
				{
					"ind1":"1",
					"ind2":"_",
					"subfields":
					[
						{
							"a":"Shakespeare, William,"
						},
						{
							"d":"1564-1616."
						}
					]
				}
			},
			{
				"245":
				{
					"ind1":"1",
					"ind2":"4",
					"subfields":
					[
						{
							"a":"The first part of King Henry the Fourth,"
						}
					]
				}
			},
			{
				"260":
				{
					"ind1":"_",
					"ind2":"_",
					"subfields":
					[
						{
							"a":"New Haven,"
						},
						{
							"b":"Yale university press; [etc., etc.]"
						},
						{
							"c":"1917."
						}
					]
				}
			},
			{
				"300":
				{
					"ind1":"_",
					"ind2":"_",
					"subfields":
					[
						{
							"a":"4 p."
						},
						{
							"b":"l., 148 p. illus. (map)"
						},
						{
							"c":"18 cm."
						}
					]
				}
			},
			{
				"530":
				{
					"ind1":"_",
					"ind2":"_",
					"subfields":
					[
						{
							"a":"Also available in digital form."
						}
					]
				}
			},
			{
				"600":
				{
					"ind1":"0",
					"ind2":"0",
					"subfields":
					[
						{
							"a":"Henry"
						},
						{
							"b":"IV,"
						},
						{
							"c":"King of England,"
						},
						{
							"d":"1367-1413"
						},
						{
							"x":"Drama."
						}
					]
				}
			},
			{
				"700":
				{
					"ind1":"1",
					"ind2":"_",
					"subfields":
					[
						{
							"a":"Hemingway, Samuel Burdett,"
						},
						{
							"d":"1883-1958,"
						},
						{
							"e":"ed."
						}
					]
				}
			},
			{
				"856":
				{
					"ind1":"4",
					"ind2":"1",
					"subfields":
					[
						{
							"u":"http://hdl.loc.gov/loc.gdc/scd0001.00139980359"
						}
					]
				}
			},
			{
				"991":
				{
					"ind1":"_",
					"ind2":"_",
					"subfields":
					[
						{
							"b":"c-GenColl"
						},
						{
							"h":"PR2810.A2"
						},
						{
							"i":"H4"
						},
						{
							"t":"Copy 1"
						},
						{
							"w":"PREM"
						}
					]
				}
			},
			{
				"985":
				{
					"ind1":"_",
					"ind2":"_",
					"subfields":
					[
						{
							"a":"massdig"
						},
						{
							"e":"sloan"
						}
					]
				}
			}
		]
	}
]
```

## Usage

In order to use the parser, you need to load marc_parser.hs and then run main with 2 command line arguments, namely the input and output file paths (both have to be existing files, the output file will be overwritten).
