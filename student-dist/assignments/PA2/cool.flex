/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <stdlib.h>
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

int comment_depth=0;		/* Variable for comment nesting */

bool max_strlen_check();
int max_strlen_err();
int valid_str();
int max_escape_char_err();

%}

/*
 * Define names for regular expressions here.
 */

WHITESPACE                [ \f\r\t\v]+
NEWLINE                   \n
SINGLES                   "+"|"-"|"*"|"/"|"~"|"<"|"="|"("|")"|"{"|"}"|";"|":"|"."|","|"@"

DASH_COMMENT              --[^\n]*
START_COMMENT             "(*"
END_COMMENT               "*)"

QUOTATION_MARK            \"
NULL_CHAR                 \0
ESCAPE_INT                \\[0-9]+
ESCAPE_CHAR               \\[^\0]
LINE_FEED_CHAR            \\n
FORM_FEED_CHAR            \\f
TAB_CHAR                  \\t
WORD_BOUNDARY             \\b

 /*
  * Case Insensitive Keywords
  */

CLASS                     (?i:class)
ELSE                      (?i:else)
FI                        (?i:fi)
IF                        (?i:if)
IN                        (?i:in)
INHERITS                  (?i:inherits)
LET                       (?i:let)
LOOP                      (?i:loop)
POOL                      (?i:pool)
THEN                      (?i:then)
WHILE                     (?i:while)
CASE                      (?i:case)
ESAC                      (?i:esac)
OF                        (?i:of)
DARROW                     =>
NEW                       (?i:new)
ISVOID                    (?i:isvoid)
INT_CONST                 [0-9]+
ASSIGN                    <-
NOT                       (?i:not)
LE                        <=

TRUE                      t(?i:rue)
FALSE                     f(?i:alse)

ID_CHARS                  [_a-zA-Z0-9]

TYPEID                    [A-Z]{ID_CHARS}*
OBJECTID                  [a-z]{ID_CHARS}*

/*
 *  Start Conditions
 */

%x COMMENT
%x STRING


%%

{WHITESPACE}              ;
{NEWLINE}                 { curr_lineno++; }
{SINGLES}                 { return *yytext; }

{DASH_COMMENT}            ;
{START_COMMENT}           { comment_depth++; BEGIN(COMMENT); }
{END_COMMENT}             { yylval.error_msg = "Unmatched *)"; return (ERROR); }

{QUOTATION_MARK}          { string_buf_ptr = string_buf; BEGIN(STRING); }

{CLASS}                   { return (CLASS); }
{ELSE}                    { return (ELSE); }
{FI}                      { return (FI); }
{IF}                      { return (IF); }
{IN}                      { return (IN); }
{INHERITS}                { return (INHERITS); }
{LET}                     { return (LET); }
{LOOP}                    { return (LOOP); }
{POOL}                    { return (POOL); }
{THEN}                    { return (THEN); }
{WHILE}                   { return (WHILE); }
{CASE}                    { return (CASE);}
{ESAC}                    { return (ESAC); }
{OF}                      { return (OF); }
{DARROW}                  { return (DARROW); }
{NEW}                     { return (NEW); }
{ISVOID}                  { return (ISVOID); }
{INT_CONST}               { yylval.symbol = inttable.add_string(yytext); return (INT_CONST);}
{ASSIGN}                  { return (ASSIGN); }
{NOT}                     { return (NOT); }
{LE}                      { return (LE); }

{TRUE}                    { yylval.boolean = true; return (BOOL_CONST); }
{FALSE}                   { yylval.boolean = false; return (BOOL_CONST); }

{TYPEID}                  { yylval.symbol = idtable.add_string(yytext); return (TYPEID); }
{OBJECTID}                { yylval.symbol = idtable.add_string(yytext); return (OBJECTID); }

.                         { yylval.error_msg = yytext; return (ERROR); }


<COMMENT>{
  <<EOF>>                 { yylval.error_msg = "EOF in comment"; BEGIN(INITIAL); return ERROR; }
  {START_COMMENT}         { comment_depth++; }
  {END_COMMENT}           { comment_depth--; if (comment_depth == 0) { BEGIN(INITIAL); } }
  {NEWLINE}               { curr_lineno++; }
  .                       ;
}

<STRING>{
  <<EOF>>                 { yylval.error_msg = "EOF in string constant"; BEGIN(INITIAL); return ERROR; }
  {QUOTATION_MARK}        { *string_buf_ptr = '\0'; return (max_strlen_check() ? max_strlen_err() : valid_str()); }
  {NULL_CHAR}             { yylval.error_msg = "String contains null character"; BEGIN(INITIAL); return ERROR; }
  {NEWLINE}               { yylval.error_msg = "Unterminated string constant"; BEGIN(INITIAL); return ERROR; }
  {WORD_BOUNDARY}         { *string_buf_ptr++ = '\b'; }
  {TAB_CHAR}              { *string_buf_ptr++ = '\t'; }
  {LINE_FEED_CHAR}        { *string_buf_ptr++ = '\n'; }
  {FORM_FEED_CHAR}        { *string_buf_ptr++ = '\f'; }
  {ESCAPE_INT}            { int num = strtol(yytext + 1, NULL, 8); if (num >= 256) return max_escape_char_err(); *string_buf_ptr++ = num; }
  {ESCAPE_CHAR}           { *string_buf_ptr++ = yytext[1]; }
  .                       { *string_buf_ptr++ = *yytext; }
}


%%

bool max_strlen_check () {
  return string_buf_ptr - string_buf >= MAX_STR_CONST;
}

int max_strlen_err() {
  yylval.error_msg = "String constant too long";
  BEGIN(INITIAL);
  return ERROR;
}

int valid_str() {
  yylval.symbol = stringtable.add_string(string_buf);
  BEGIN(INITIAL);
  return STR_CONST;
}

int max_escape_char_err() {
  yylval.error_msg = "Large escape int character";
  BEGIN(INITIAL);
  return ERROR;
}
