grammar Cobol85;

// Basic structure
compilationUnit: programUnit+ EOF;

programUnit: identificationDivision environmentDivision? dataDivision? procedureDivision?;

// Identification Division
identificationDivision: IDENTIFICATION DIVISION DOT programIdParagraph;
programIdParagraph: PROGRAM_ID DOT programName DOT;
programName: IDENTIFIER;

// Environment Division
environmentDivision: ENVIRONMENT DIVISION DOT configurationSection? inputOutputSection?;
configurationSection: CONFIGURATION SECTION DOT (sourceComputerParagraph | objectComputerParagraph | specialNamesParagraph)*;
inputOutputSection: INPUT_OUTPUT SECTION DOT fileControlParagraph? iOControlParagraph?;
sourceComputerParagraph: SOURCE_COMPUTER DOT computerName DOT;
objectComputerParagraph: OBJECT_COMPUTER DOT computerName DOT;
specialNamesParagraph: SPECIAL_NAMES DOT specialNameEntry* DOT;
fileControlParagraph: FILE_CONTROL DOT fileControlEntry* DOT;
iOControlParagraph: I_O_CONTROL DOT iOControlEntry* DOT;
computerName: IDENTIFIER;
specialNameEntry: (mnemonicName IS mnemonicName | alphabetName IS alphabetName | symbolicCharactersClause | classClause | currencySignClause | decimalPointClause)+;
fileControlEntry: SELECT fileName assignClause? fileStatusClause? DOT;
iOControlEntry: rerunClause | sameClause | multipleFileClause;
mnemonicName: IDENTIFIER;
alphabetName: IDENTIFIER;
symbolicCharactersClause: SYMBOLIC CHARACTERS (symbolicCharacter+ (ARE | IS) integer+)+ (IN alphabetName)?;
classClause: CLASS className IS (literal | IDENTIFIER)+;
currencySignClause: CURRENCY SIGN IS literal;
decimalPointClause: DECIMAL_POINT IS COMMA;
assignClause: ASSIGN TO (IDENTIFIER | literal);
fileStatusClause: FILE STATUS IS IDENTIFIER;
rerunClause: RERUN ON (fileSystem | mnemonicName) EVERY (integer RECORDS | END OF (REEL | UNIT) OF fileName);
sameClause: SAME (RECORD | SORT | SORT_MERGE)? AREA FOR fileName+;
multipleFileClause: MULTIPLE FILE TAPE CONTAINS (fileName (POSITION integer)?)+;
fileSystem: IDENTIFIER;
className: IDENTIFIER;
symbolicCharacter: IDENTIFIER;


// Data Division
dataDivision: DATA DIVISION DOT (fileSection | workingStorageSection | linkageSection)*;
fileSection: FILE SECTION DOT fileDescriptionEntry*;
fileDescriptionEntry: FD fileName (blockContainsClause | recordContainsClause | labelRecordsClause | valueOfClause | dataRecordClause | linageClause | codeSetClause)* DOT dataDescriptionEntry*;
workingStorageSection: WORKING_STORAGE SECTION DOT dataDescriptionEntry*;
linkageSection: LINKAGE SECTION DOT dataDescriptionEntry*;

blockContainsClause: BLOCK CONTAINS (integer TO)? integer (RECORDS | CHARACTERS)?;
recordContainsClause: RECORD CONTAINS (integer TO)? integer CHARACTERS;
labelRecordsClause: LABEL RECORDS (ARE | IS) (STANDARD | OMITTED | dataName+);
valueOfClause: VALUE OF (systemName IS (dataName | literal))+;
dataRecordClause: DATA (RECORD IS | RECORDS ARE) dataName+;
linageClause: LINAGE IS (dataName | integer) LINES (WITH FOOTING AT (dataName | integer))? (LINES AT TOP (dataName | integer))? (LINES AT BOTTOM (dataName | integer))?;
codeSetClause: CODE_SET IS alphabetName;
systemName: IDENTIFIER;
dataName: IDENTIFIER;


dataDescriptionEntry: levelNumber (dataName | FILLER)? (redefinesClause | blankWhenZeroClause | externalClause | globalClause | justifiedClause | occursClause | pictureClause | signClause | synchronizedClause | usageClause | valueClause)* DOT;
levelNumber: INTEGER;
redefinesClause: REDEFINES dataName;
blankWhenZeroClause: BLANK WHEN ZERO;
externalClause: EXTERNAL;
globalClause: GLOBAL;
justifiedClause: (JUSTIFIED | JUST) RIGHT;
occursClause: OCCURS (integer TO)? integer TIMES (DEPENDING ON dataName)? (ASCENDING | DESCENDING)? (KEY IS dataName+)? (INDEXED BY indexName+)?;
pictureClause: (PICTURE | PIC) IS? pictureString;
signClause: SIGN IS? (LEADING | TRAILING) (SEPARATE CHARACTER)?;
synchronizedClause: (SYNCHRONIZED | SYNC) (LEFT | RIGHT)?;
usageClause: (USAGE IS?) (BINARY | COMP | COMP_1 | COMP_2 | COMP_3 | COMP_4 | COMP_5 | COMPUTATIONAL | COMPUTATIONAL_1 | COMPUTATIONAL_2 | COMPUTATIONAL_3 | COMPUTATIONAL_4 | COMPUTATIONAL_5 | DISPLAY | DISPLAY_1 | INDEX | PACKED_DECIMAL | POINTER | PROCEDURE_POINTER);
valueClause: VALUE IS? literal;
indexName: IDENTIFIER;
pictureString: (IDENTIFIER | INTEGER | LPAREN | RPAREN)+;

// Procedure Division
procedureDivision: PROCEDURE DIVISION (USING dataName+)? DOT paragraphs;
paragraphs: (paragraph | section)*;
section: sectionHeader paragraph*;
sectionHeader: sectionName SECTION priorityNumber? DOT;
sectionName: IDENTIFIER;
priorityNumber: INTEGER;
paragraph: paragraphName DOT sentence*;
paragraphName: IDENTIFIER;
sentence: statement* DOT;

statement: displayStatement
         | moveStatement
         | addStatement
         | subtractStatement
         | multiplyStatement
         | divideStatement
         | computeStatement
         | performStatement
         | ifStatement
         | evaluateStatement
         | callStatement
         | copyStatement
         | stopStatement
         | goStatement
         | exitStatement
         | nextSentenceStatement
         | openStatement
         | closeStatement
         | readStatement
         | writeStatement
         | rewriteStatement
         | deleteStatement
         | startStatement
         | unstringStatement
         | stringStatement
         | inspectStatement
         | searchStatement
         | setStatement
         | continueStatement
         ;

displayStatement: DISPLAY (IDENTIFIER | literal)+ (UPON mnemonicName)? (WITH NO ADVANCING)?;
moveStatement: MOVE (IDENTIFIER | literal) TO IDENTIFIER+;
addStatement: ADD (IDENTIFIER | literal)+ TO IDENTIFIER+ (GIVING IDENTIFIER+)?;
subtractStatement: SUBTRACT (IDENTIFIER | literal)+ FROM IDENTIFIER+ (GIVING IDENTIFIER+)?;
multiplyStatement: MULTIPLY (IDENTIFIER | literal) BY IDENTIFIER (GIVING IDENTIFIER+)?;
divideStatement: DIVIDE (IDENTIFIER | literal) INTO IDENTIFIER (GIVING IDENTIFIER+)? (REMAINDER IDENTIFIER)?;
computeStatement: COMPUTE IDENTIFIER+ EQUAL arithmeticExpression;
performStatement: PERFORM (paragraphName | sectionName) (THROUGH (paragraphName | sectionName))? ((integer | IDENTIFIER) TIMES | UNTIL condition | VARYING IDENTIFIER FROM (integer | IDENTIFIER) BY (integer | IDENTIFIER) UNTIL condition)?;
ifStatement: IF condition THEN? (statement | NEXT SENTENCE)+ (ELSE (statement | NEXT SENTENCE)+)? END_IF?;
evaluateStatement: EVALUATE (IDENTIFIER | literal | condition) (ALSO (IDENTIFIER | literal | condition))* (WHEN (ANY | condition | TRUE | FALSE | (NOT)? (IDENTIFIER | literal) (THROUGH (IDENTIFIER | literal))?) (ALSO (ANY | condition | TRUE | FALSE | (NOT)? (IDENTIFIER | literal) (THROUGH (IDENTIFIER | literal))?))* statement+)+ (WHEN OTHER statement+)? END_EVALUATE;
callStatement: CALL (IDENTIFIER | literal) (USING (BY REFERENCE | BY CONTENT | BY VALUE)? (IDENTIFIER | literal)+)* (RETURNING IDENTIFIER)? (ON OVERFLOW statement+)? (END_CALL)?;
copyStatement: COPY (textName | literal) (OF | IN libraryName)? (SUPPRESSING)? (REPLACING (operand BY operand)+)?;
stopStatement: STOP (RUN | literal);
goStatement: GO TO (paragraphName | sectionName)+ (DEPENDING ON IDENTIFIER)?;
exitStatement: EXIT (PROGRAM | PERFORM | SECTION | PARAGRAPH)?;
nextSentenceStatement: NEXT SENTENCE;
openStatement: OPEN (INPUT | OUTPUT | I_O | EXTEND) fileName+;
closeStatement: CLOSE (fileName (WITH (NO REWIND | LOCK))?)+;
readStatement: READ fileName (NEXT)? (RECORD)? (INTO IDENTIFIER)? (KEY IS dataName)? (INVALID KEY statement+)? (NOT INVALID KEY statement+)? (AT END statement+)? (NOT AT END statement+)? END_READ?;
writeStatement: WRITE recordName (FROM IDENTIFIER)? (BEFORE | AFTER) (ADVANCING ((integer | IDENTIFIER) (LINE | LINES) | PAGE | mnemonicName))? (INVALID KEY statement+)? (NOT INVALID KEY statement+)? (END_OF_PAGE statement+)? (NOT END_OF_PAGE statement+)? END_WRITE?;
rewriteStatement: REWRITE recordName (FROM IDENTIFIER)? (INVALID KEY statement+)? (NOT INVALID KEY statement+)? END_REWRITE?;
deleteStatement: DELETE fileName (RECORD)? (INVALID KEY statement+)? (NOT INVALID KEY statement+)? END_DELETE?;
startStatement: START fileName (KEY (IS)? (EQUAL TO | EQ | GREATER THAN | GT | NOT LESS THAN | NOT LT | GREATER THAN OR EQUAL TO | GTE) dataName)? (INVALID KEY statement+)? (NOT INVALID KEY statement+)? END_START?;
unstringStatement: UNSTRING IDENTIFIER (DELIMITED BY (ALL)? (IDENTIFIER | literal) (OR (ALL)? (IDENTIFIER | literal))*)? INTO (IDENTIFIER (DELIMITER IN IDENTIFIER)? (COUNT IN IDENTIFIER)?)+ (WITH POINTER IDENTIFIER)? (TALLYING IN IDENTIFIER)? (ON OVERFLOW statement+)? (NOT ON OVERFLOW statement+)? END_UNSTRING?;
stringStatement: STRING (IDENTIFIER | literal)+ DELIMITED BY (SIZE | IDENTIFIER | literal) INTO IDENTIFIER (WITH POINTER IDENTIFIER)? (ON OVERFLOW statement+)? (NOT ON OVERFLOW statement+)? END_STRING?;
inspectStatement: INSPECT IDENTIFIER (TALLYING (IDENTIFIER FOR (CHARACTERS (BEFORE | AFTER INITIAL (IDENTIFIER | literal))? | (ALL | LEADING) (IDENTIFIER | literal) (BEFORE | AFTER INITIAL (IDENTIFIER | literal))?)+)+ | REPLACING (CHARACTERS BY (IDENTIFIER | literal) (BEFORE | AFTER INITIAL (IDENTIFIER | literal))? | (ALL | LEADING | FIRST) (IDENTIFIER | literal) BY (IDENTIFIER | literal) (BEFORE | AFTER INITIAL (IDENTIFIER | literal))?)+ | CONVERTING (IDENTIFIER | literal) TO (IDENTIFIER | literal) (BEFORE | AFTER INITIAL (IDENTIFIER | literal))?);
searchStatement: SEARCH (ALL)? IDENTIFIER (VARYING IDENTIFIER)? (AT END statement+)? (WHEN condition (statement | NEXT SENTENCE)+)+ END_SEARCH?;
setStatement: SET (IDENTIFIER+ TO (IDENTIFIER | literal | TRUE | FALSE | ON | OFF) | (IDENTIFIER)+ (UP | DOWN) BY (integer | IDENTIFIER));
continueStatement: CONTINUE;

textName: IDENTIFIER;
libraryName: IDENTIFIER;
operand: IDENTIFIER | literal;
recordName: IDENTIFIER;
fileName: IDENTIFIER;

arithmeticExpression: term ((PLUS | MINUS) term)*;
term: factor ((MULTIPLY | DIVIDE) factor)*;
factor: (PLUS | MINUS)? (IDENTIFIER | literal | LPAREN arithmeticExpression RPAREN);

condition: combinableCondition ((AND | OR) combinableCondition)*;
combinableCondition: (NOT)? simpleCondition;
simpleCondition: relationCondition | classCondition | signCondition | conditionNameCondition | switchStatusCondition | LPAREN condition RPAREN;
relationCondition: arithmeticExpression relationalOperator arithmeticExpression;
relationalOperator: GREATER (THAN)? | LESS (THAN)? | EQUAL (TO)? | GREATER (THAN)? OR EQUAL (TO)? | LESS (THAN)? OR EQUAL (TO)? | NOT GREATER (THAN)? | NOT LESS (THAN)? | NOT EQUAL (TO)? | GT | LT | EQ | GTE | LTE | NEQ;
classCondition: IDENTIFIER IS (NOT)? (NUMERIC | ALPHABETIC | ALPHABETIC_LOWER | ALPHABETIC_UPPER | className);
signCondition: arithmeticExpression IS (NOT)? (POSITIVE | NEGATIVE | ZERO);
conditionNameCondition: conditionName;
conditionName: IDENTIFIER;
switchStatusCondition: conditionName;

// Lexer Rules
IDENTIFICATION: 'IDENTIFICATION';
DIVISION: 'DIVISION';
PROGRAM_ID: 'PROGRAM-ID';
ENVIRONMENT: 'ENVIRONMENT';
CONFIGURATION: 'CONFIGURATION';
INPUT_OUTPUT: 'INPUT-OUTPUT';
SECTION: 'SECTION';
SOURCE_COMPUTER: 'SOURCE-COMPUTER';
OBJECT_COMPUTER: 'OBJECT-COMPUTER';
SPECIAL_NAMES: 'SPECIAL-NAMES';
FILE_CONTROL: 'FILE-CONTROL';
I_O_CONTROL: 'I-O-CONTROL';
DATA: 'DATA';
FILE: 'FILE';
WORKING_STORAGE: 'WORKING-STORAGE';
LINKAGE: 'LINKAGE';
PROCEDURE: 'PROCEDURE';
USING: 'USING';
DISPLAY: 'DISPLAY';
MOVE: 'MOVE';
TO: 'TO';
ADD: 'ADD';
SUBTRACT: 'SUBTRACT';
MULTIPLY: 'MULTIPLY';
DIVIDE: 'DIVIDE';
COMPUTE: 'COMPUTE';
PERFORM: 'PERFORM';
IF: 'IF';
THEN: 'THEN';
ELSE: 'ELSE';
END_IF: 'END-IF';
EVALUATE: 'EVALUATE';
WHEN: 'WHEN';
ALSO: 'ALSO';
ANY: 'ANY';
TRUE: 'TRUE';
FALSE: 'FALSE';
NOT: 'NOT';
OTHER: 'OTHER';
END_EVALUATE: 'END-EVALUATE';
CALL: 'CALL';
RETURNING: 'RETURNING';
ON: 'ON';
OVERFLOW: 'OVERFLOW';
END_CALL: 'END-CALL';
COPY: 'COPY';
OF: 'OF';
IN: 'IN';
SUPPRESSING: 'SUPPRESSING';
REPLACING: 'REPLACING';
BY: 'BY';
STOP: 'STOP';
RUN: 'RUN';
GO: 'GO';
EXIT: 'EXIT';
NEXT: 'NEXT';
SENTENCE: 'SENTENCE';
OPEN: 'OPEN';
CLOSE: 'CLOSE';
READ: 'READ';
WRITE: 'WRITE';
REWRITE: 'REWRITE';
DELETE: 'DELETE';
START: 'START';
UNSTRING: 'UNSTRING';
STRING: 'STRING';
INSPECT: 'INSPECT';
SEARCH: 'SEARCH';
SET: 'SET';
CONTINUE: 'CONTINUE';
FD: 'FD';
BLOCK: 'BLOCK';
CONTAINS: 'CONTAINS';
RECORDS: 'RECORDS';
CHARACTERS: 'CHARACTERS';
LABEL: 'LABEL';
STANDARD: 'STANDARD';
OMITTED: 'OMITTED';
VALUE: 'VALUE';
LINAGE: 'LINAGE';
WITH: 'WITH';
FOOTING: 'FOOTING';
AT: 'AT';
TOP: 'TOP';
BOTTOM: 'BOTTOM';
CODE_SET: 'CODE-SET';
REDEFINES: 'REDEFINES';
BLANK: 'BLANK';
ZERO: 'ZERO';
EXTERNAL: 'EXTERNAL';
GLOBAL: 'GLOBAL';
JUSTIFIED: 'JUSTIFIED';
JUST: 'JUST';
RIGHT: 'RIGHT';
OCCURS: 'OCCURS';
TIMES: 'TIMES';
DEPENDING: 'DEPENDING';
ASCENDING: 'ASCENDING';
DESCENDING: 'DESCENDING';
KEY: 'KEY';
INDEXED: 'INDEXED';
PICTURE: 'PICTURE';
PIC: 'PIC';
SIGN: 'SIGN';
LEADING: 'LEADING';
TRAILING: 'TRAILING';
SEPARATE: 'SEPARATE';
CHARACTER: 'CHARACTER';
SYNCHRONIZED: 'SYNCHRONIZED';
SYNC: 'SYNC';
LEFT: 'LEFT';
USAGE: 'USAGE';
BINARY: 'BINARY';
COMP: 'COMP';
COMP_1: 'COMP-1';
COMP_2: 'COMP-2';
COMP_3: 'COMP-3';
COMP_4: 'COMP-4';
COMP_5: 'COMP-5';
COMPUTATIONAL: 'COMPUTATIONAL';
COMPUTATIONAL_1: 'COMPUTATIONAL-1';
COMPUTATIONAL_2: 'COMPUTATIONAL-2';
COMPUTATIONAL_3: 'COMPUTATIONAL-3';
COMPUTATIONAL_4: 'COMPUTATIONAL-4';
COMPUTATIONAL_5: 'COMPUTATIONAL-5';
DISPLAY_1: 'DISPLAY-1';
INDEX: 'INDEX';
PACKED_DECIMAL: 'PACKED-DECIMAL';
POINTER: 'POINTER';
PROCEDURE_POINTER: 'PROCEDURE-POINTER';
IS: 'IS';
ARE: 'ARE';
FROM: 'FROM';
GIVING: 'GIVING';
INTO: 'INTO';
REMAINDER: 'REMAINDER';
EQUAL: 'EQUAL';
THROUGH: 'THROUGH';
UNTIL: 'UNTIL';
VARYING: 'VARYING';
REFERENCE: 'REFERENCE';
CONTENT: 'CONTENT';
INPUT: 'INPUT';
OUTPUT: 'OUTPUT';
I_O: 'I-O';
EXTEND: 'EXTEND';
REWIND: 'REWIND';
LOCK: 'LOCK';
INVALID: 'INVALID';
END: 'END';
ADVANCING: 'ADVANCING';
LINE: 'LINE';
LINES: 'LINES';
PAGE: 'PAGE';
END_OF_PAGE: 'END-OF-PAGE';
END_READ: 'END-READ';
END_WRITE: 'END-WRITE';
END_REWRITE: 'END-REWRITE';
END_DELETE: 'END-DELETE';
END_START: 'END-START';
END_UNSTRING: 'END-UNSTRING';
END_STRING: 'END-STRING';
END_SEARCH: 'END-SEARCH';
DELIMITED: 'DELIMITED';
OR: 'OR';
DELIMITER: 'DELIMITER';
COUNT: 'COUNT';
TALLYING: 'TALLYING';
SIZE: 'SIZE';
INITIAL: 'INITIAL';
FIRST: 'FIRST';
CONVERTING: 'CONVERTING';
UP: 'UP';
DOWN: 'DOWN';
EQ: '=';
GT: '>';
LT: '<';
GTE: '>=';
LTE: '<=';
NEQ: '<>';
PLUS: '+';
MINUS: '-';
ASTERISK: '*';
SLASH: '/';
LPAREN: '(';
RPAREN: ')';
DOT: '.';
COMMA: ',';
GREATER: 'GREATER';
LESS: 'LESS';
NUMERIC: 'NUMERIC';
ALPHABETIC: 'ALPHABETIC';
ALPHABETIC_LOWER: 'ALPHABETIC-LOWER';
ALPHABETIC_UPPER: 'ALPHABETIC-UPPER';
POSITIVE: 'POSITIVE';
NEGATIVE: 'NEGATIVE';

IDENTIFIER: [a-zA-Z][a-zA-Z0-9-]*;
INTEGER: [0-9]+;
STRING_LITERAL: '"' .*? '"' | '\'' .*? '\'';
WS: [ \t\r\n]+ -> skip;
COMMENT: '*' .*? '\r'? '\n' -> skip;


literal: INTEGER | STRING_LITERAL | ZERO | TRUE | FALSE;
integer: INTEGER;

