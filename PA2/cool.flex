/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
* to the code in the file.  Don't remove anything that was here initially
*/

%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>
#include <string>

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
static int comment_caller;
static int string_caller;
std::string str_buf;
static int string_error;
%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
CLASS           class
ELSE            else
FI              fi
IF              if
IN              in
INHERITS        inherits
LET             let
LOOP            loop
POOL            pool
THEN            then
WHILE           while
CASE            case
ESAC            esac
OF              of
NEW             new
ISVOID          ISVOID
ASSIGN          <-
NOT             not
LE              <=

%x COMMENT
%x STRING
%x STRING_ESCAPE

%%

 /*
  *  Nested comments
  */
--.*$             {}

"(*"              { 
  comment_caller = YY_START;
  BEGIN(COMMENT); 
  }
  /* Should not tokenize anything inside the comment, but still increase the lineno */
<COMMENT>[^(\*\))]     { 
  if(yytext[0] == '\n') curr_lineno++;
  }

<COMMENT>"*)"     BEGIN(comment_caller);

  /* Closing comment without opening */
"*)"              {
  yylval.error_msg = "Unmatched *)";
  return (ERROR);
  }
  /* EOF in the middle of comment */
<COMMENT><<EOF>>  { 
  yylval.error_msg = "EOF in comment";
  BEGIN(comment_caller);
  return (ERROR);
  }

 /*
  *  The multiple-character operators.
  */

[\[\]'>]          {
  yylval.error_msg = yytext;
  return (ERROR);
  }
[ \f\r\t\v]       {}
\n                { curr_lineno++; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must BEGIN with a lower-case letter.
  */
{DARROW}		{ return (DARROW); }
{CLASS}		  { return (CLASS); }
{ELSE}		  { return (ELSE); }
{FI}		    { return (FI); }
{IF}		    { return (IF); }
{IN}		    { return (IN); }
{INHERITS}	{ return (INHERITS); }
{LET}		    { return (LET); }
{LOOP}		  { return (LOOP); }
{POOL}		  { return (POOL); }
{THEN}		  { return (THEN); }
{WHILE}		  { return (WHILE); }
{CASE}		  { return (CASE); }
{ESAC}		  { return (ESAC); }
{OF}		    { return (OF); }
{NEW}		    { return (NEW); }
{ISVOID}		{ return (ISVOID); }
{ASSIGN}		{ return (ASSIGN); }
{NOT}		    { return (NOT); }
{LE}		    { return (LE); }

  /* true and false */
t[Rr][Uu][Ee]       { 
  yylval.boolean = true;
  return (BOOL_CONST);
  }
f[Aa][Ll][Ss][Ee]   { 
  yylval.boolean = false;
  return (BOOL_CONST);
  }
  /* identifier */
  /* typeID */
[A-Z][A-Za-z0-9_]*  {
  yylval.symbol = idtable.add_string(yytext, yyleng);
  return (TYPEID);
  }
  /* objectID */
[a-z][A-Za-z0-9_]*  {  
  yylval.symbol = idtable.add_string(yytext, yyleng);
  return (OBJECTID);
  }
  /* integer */
[0-9]+              {
  yylval.symbol = inttable.add_int(atoi(yytext));
  return (INT_CONST);
  }

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
\"                    {
  string_caller = YY_START;
  string_error = 0;
  str_buf.erase(0);
  BEGIN(STRING); 
  }
  /* read the entire string until reach closing, need extra buffer to contain the string for there may be 
   * escape charater that interupt the parse 
  */
<STRING>'\0'            {
  if(string_error == 0) {
    string_error = 1;
    yylval.error_msg = "String contains null character";
    //return (ERROR);
  }
  }
<STRING><<EOF>>       {
  if(string_error == 0) {
    string_error = 1;
    yylval.error_msg = "String contains EOF";
    //return (ERROR);
  }
  }
<STRING>[^\"\\\0<<EOF>>]*'\n'    {
  // consider end of the string where unescaped newline is in the string
  str_buf.append(yytext, yyleng - 1); // does not append the last \n
  if(string_error == 0) {
    string_error = 1;
    yylval.error_msg = "Unterminated string constant";
  }
  BEGIN(string_caller);
  curr_lineno++;
  return (ERROR);
  }
<STRING>[^\"\\\0<<EOF>>]*\"    {
  str_buf.append(yytext, yyleng - 1); // does not append the last "
  yylval.symbol = stringtable.add_string(&str_buf[0], str_buf.size()); // c_str() end with \0
  BEGIN(string_caller);
  if(string_error == 1) return (ERROR);
  return (STR_CONST);
  }
<STRING>[^\"\\]*\\    {
  str_buf.append(yytext, yyleng - 1);
  BEGIN(STRING_ESCAPE);
  }
<STRING_ESCAPE>b      {
  str_buf += '\b';
  BEGIN(STRING);
  }
<STRING_ESCAPE>t      {
  str_buf += '\t';
  BEGIN(STRING);
  }
<STRING_ESCAPE>f      {
  str_buf += '\f';
  BEGIN(STRING);
  }
<STRING_ESCAPE>n      {
  str_buf += '\n';
  BEGIN(STRING);
  }
<STRING_ESCAPE>'\n'     {
  // only escaped new line is valid
  str_buf += '\n';
  curr_lineno++;
  BEGIN(STRING);
  }
<STRING_ESCAPE>'\0'            {
  if(string_error == 0) {
    string_error = 1;
    yylval.error_msg = "String contains null character";
    BEGIN(STRING);
    //return (ERROR);
  }
  }
<STRING_ESCAPE>0      {
  // vaild for two charaters \ and 0, not valid for the literal null: \0
  str_buf += "0";
  BEGIN(STRING);
  }
<STRING_ESCAPE><<EOF>>       {
  if(string_error == 0) {
    string_error = 1;
    yylval.error_msg = "String contains EOF";
    BEGIN(STRING);
    //return (ERROR);
  }
  }
<STRING_ESCAPE>.      {
  str_buf += yytext[0];
  BEGIN(STRING);
  }

.                     { return yytext[0]; }

%%
