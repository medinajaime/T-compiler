%{
	#include <stdio.h>
	#include <stdlib.h>
	#include "t2c.h"
	#include "t_parse.h"
%}

%token lWRITE lREAD lIF lASSIGN
%token lRETURN lBEGIN lEND
%left  lEQU lNEQ lGT lLT lGE lLE
%left  lADD lMINUS
%left  lTIMES lDIVIDE
%token lLP lRP
%token lINT lREAL lSTRING
%token lELSE
%token lMAIN
%token lSEMI lCOMMA
%token lID lINUM lRNUM lQSTR

%expect 1

%%
prog	:	mthdcls
		{ printf("Program -> MethodDecls\n");
		  printf("Parsed OK!\n"); }
	|
		{ printf("****** Parsing failed!\n"); }	
	;

mthdcls	:	mthdcl mthdcls
		{ printf("MethodDecls -> MethodDecl MethodDecls\n"); }	
	|	mthdcl
		{ printf("MethodDecls -> MethodDecl\n"); }	
	;

type	:	lINT
		{ printf("Type -> INT\n"); }	
	|	lREAL
		{ printf("Type -> REAL\n"); }	
	;

mthdcl	:	type lMAIN lID lLP formals lRP block
		{ printf("MethodDecl -> Type MAIN ID LP Formals RP Block\n"); }	
	|	type lID lLP formals lRP block
		{ printf("MethodDecl -> Type ID LP Formals RP Block\n"); }	
	;

formals	:	formal oformal
		{ printf("Formals -> Formal OtherFormals\n"); }	
	|
		{ printf("Formals -> \n"); }	
	;

formal	:	type lID
		{ printf("Formal -> Type ID\n"); }	
	;

oformal	:	lCOMMA formal oformal
		{ printf("OtherFormals -> COMMA Formal OtherFormals\n"); }	
	|
		{ printf("OtherFormals -> \n"); }	
	;

// Statements and Expressions

block : lBEGIN stmtList lEND
	{ printf("Block -> BEGIN Statement+ END\n");}

stmtList : stmt stmtList 
		{printf("StatementList -> Statement StatementList\n");}
		| stmt 
		{printf("StatementList -> Statement\n");}
		;

stmt :  block
		{printf("Statement -> Block\n");}
		| localVarDecl
		{printf("Statement -> LocalVarDecl\n");}
		| assignStmt
		{printf("Statement -> AssignStmt\n");}
		| returnStmt
		{printf("Statement -> ReturnStmt\n");}
		| ifStmt
		{printf("Statement -> IfStmt\n");}
		| writeStmt
		{printf("Statement -> WriteStmt\n");}
		| readStmt
		{printf("Statement -> ReadStmt\n");}
		;

localVarDecl : type lID lSEMI 
			{printf("LocalVarDecl -> Type Id SEMI\n");}
			| type assignStmt 
			{printf("LocalVarDecl -> Type AssignStmt\n");}
			;

assignStmt : lID lASSIGN expr lSEMI
			{printf("AssignStmt -> ID ASSIGN Expression SEMI\n");}
			;

returnStmt : lRETURN expr lSEMI
			{printf("ReturnStmt -> RETURN Expression SEMI\n");}
			;

ifStmt : lIF lLP boolExpr lRP stmt
			{printf("IfStmt -> IF LP BoolExpr RP Statement\n");}
			| lif lLP boolExpr lRP stmt lELSE stmt
			{printf("IfStmt -> IF LP BoolExpre RP Statement ELSE Statement\n");}
			;

writeStmt : lWrite lLP expr lCOMMA lQSTR lRP lSEMI
			{printf("WriteStmt -> WRITE LP Expression COMMA QSTR RP SEMI\n");}
			;

readStmt : lREAD lLP lID lCOMMA lQSTR lRP lSEMI
			{printf("ReadStmt -> READ LP ID COMMA QSTR RP SEMI\n");}
			;

expr : multExpr
        	{ printf("Expression -> MultiplicativeExpr\n"); }
			| expr lADD multExpr
        	{ printf("Expression -> Expression + MultiplicativeExpr\n"); }
    		| expr lMINUS multExpr
        	{ printf("Expression -> Expression - MultiplicativeExpr\n"); }
    		;

multExpr : primaryExpr
        	{ printf("MultiplicativeExpr -> PrimaryExpr\n"); }
    		| multExpr lTIMES primaryExpr
        	{ printf("MultiplicativeExpr -> MultiplicativeExpr * PrimaryExpr\n"); }
    		| multExpr lDIVIDE primaryExpr
        	{ printf("MultiplicativeExpr -> MultiplicativeExpr / PrimaryExpr\n"); }
    		;

primaryExpr : lINUM
        	{ printf("PrimaryExpr -> INUM\n"); }
    		| lRNUM
        	{ printf("PrimaryExpr -> RNUM\n"); }
    		| lID
        	{ printf("PrimaryExpr -> ID\n"); }
    		| lLP expr lRP
        	{ printf("PrimaryExpr -> ( Expression )\n"); }
    		| lID lLP actualParams lRP
        	{ printf("PrimaryExpr -> ID ( ActualParams )\n"); }
    		;

boolExpr : expr lEQU expr
        	{ printf("BoolExpr -> Expression == Expression\n"); }
    		| expr lNEQ expr
        	{ printf("BoolExpr -> Expression != Expression\n"); }
    		| expr lGT expr
        	{ printf("BoolExpr -> Expression > Expression\n"); }
    		| expr lGE expr
        	{ printf("BoolExpr -> Expression >= Expression\n"); }
    		| expr lLT expr
        	{ printf("BoolExpr -> Expression < Expression\n"); }
    		| expr lLE expr
        	{ printf("BoolExpr -> Expression <= Expression\n"); }
    		;

actualParams : expr  lCOMMA actualParams
			{printf("ActualParams ->Expression  lCOMMA ActualParams\n");}
			| expr
			{printf("ActualParams ->Expression\n");}
			;


%%

int yyerror(char *s)
{
	printf("%s\n",s);
	return 1;
}

