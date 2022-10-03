%{
  #include <stdio.h>
  #include <string.h>
  #include <stdlib.h>
  void yyerror(const char* message);
  int yylex();
  int error = 0;
  int fun_num = 0;
  int fpos = 1, ppos = 1;
  int var_total = 0;
  int fun_total = 1;

  //set the type of the node
  enum parentType{
    num_type, plus_ope, minus_ope, mul_ope, div_ope, mod_ope,
    and_ope, or_ope, not_ope, eq_ope,
    great_ope, small_ope,
    var_de, fun_de
  };
  
  //set the structure of the tree
  typedef struct tree{
    enum parentType type;
    char* var_name;
    int      value;
    struct tree* left_child;
    struct tree* right_child;
  }build_tree;

  typedef struct variable_declare{
    char* var_name;
    int value;
    struct tree* tree;
  }va_declare;
  
  
  //store the name of variable information
  va_declare var[500];

  //store the name of funtion information
  va_declare fun_var[500];

  //store the parameter
  int parameter[500];
  
  build_tree *setNode(enum parentType type, char *name, int value);
  int getAns(build_tree *root);
  int getVarPlace(char* name);
  void set_fun(char* c, int pos);
  int getFunPlace(char* c);
  void get_param(build_tree *tree);
  
%}



%union{
  int ival;
  char* str;
  struct tree* tree;
}

  

%type   <tree> exp variable num_op logical_op fun_exp fun_call if_exp
%type   <tree> plus minus multiply divide modulus greater smaller equal
%type   <tree> and_op or_op not_op
%type   <tree> fun_ids fun_body
%type   <tree> test_exp then_exp else_exp
%type   <tree> exps_and exps_equal exps_mul exps_or exps_plus

%type   <ival> params param fun_name
%type   <str>  ids
%type   <tree> def_stmt

%token  <ival> number bool_val
%token  <str>  id

%token  define lambda IF printnum printbool fun
%token  mod and or not






%%

prog                :stmts
					;

stmts               :stmts stmt
                    |stmt
                    ;

stmt                :exp
                    |def_stmt
                    |print_stmt
                    ;

print_stmt     		:'(' printnum exp ')'   {
						int tmp = getAns($3);
						if(error == 0) printf("%d\n", tmp);
					}
                    |'(' printbool exp ')' {
						if(error == 0) {
							int tmp = getAns($3);
							if(tmp == 0) printf("#f\n");
							else printf("#t\n");
						}
					}
                    ;

exp                	:bool_val {
						build_tree* newTree = setNode(num_type, "", $1);
						$$ = newTree;
					}
                    |number {
						build_tree* newTree = setNode(num_type, "", $1);
						$$ = newTree;
					}
                    |variable    {$$ = $1;}
                    |num_op      {$$ = $1;}
                    |logical_op  {$$ = $1;}
                    |fun_exp     {$$ = $1;}
                    |fun_call    {$$ = $1;}
                    |if_exp      {$$ = $1;}
                    ;

num_op          	:plus		 {$$ = $1;}
                    |minus		 {$$ = $1;}
                    |multiply    {$$ = $1;}
                    |divide		 {$$ = $1;}
                    |modulus     {$$ = $1;}
                    |greater     {$$ = $1;}
                    |smaller     {$$ = $1;}
                    |equal       {$$ = $1;}
                    ;

    plus                :'(' '+' exp exps_plus ')' {
							build_tree* newTree = setNode(plus_ope, "", 0);
							newTree -> left_child = $3;
							newTree -> right_child = $4;
							$$ = newTree;
						}
						;
						
        exps_plus     	:exp {
							$$ = $1;
						}
                        |exps_plus exp {
							build_tree* newTree = setNode(plus_ope, "", 0);
							newTree -> left_child = $1;
							newTree -> right_child = $2;
							$$ = newTree;
						}
                        ;

    minus               :'(' '-' exp exp ')' {
							build_tree* newTree = setNode(minus_ope, "", 0);
							newTree -> left_child = $3;
							newTree -> right_child = $4;
							$$ = newTree;
						}

    multiply           	:'(' '*' exp exps_mul ')' {
							build_tree* newTree = setNode(mul_ope, "", 0);
							newTree -> left_child = $3;
							newTree -> right_child = $4;
							$$ = newTree;
						}
						;
						
        exps_mul     	:exp {$$ = $1;}
                        |exps_mul exp {
							build_tree* newTree = setNode(mul_ope, "", 0);
							newTree -> left_child = $1;
							newTree -> right_child = $2;
							$$ = newTree;
						}
                        ;

    divide              :'(' '/' exp exp ')' {
							build_tree* newTree = setNode(div_ope, "", 0);
							newTree -> left_child = $3;
							newTree -> right_child = $4;
							$$ = newTree;
						}
						;

    modulus           	:'(' mod exp exp ')' {
							build_tree* newTree = setNode(mod_ope, "", 0);
							newTree -> left_child = $3;
							newTree -> right_child = $4;
							$$ = newTree;
						}
						;
    
    greater            	:'(' '>' exp exp ')' {
							build_tree* newTree = setNode(great_ope, "", 0);
							newTree -> left_child = $3;
							newTree -> right_child = $4;
							$$ = newTree;
						}
						;

    smaller            	:'(' '<' exp exp ')' {
							build_tree* newTree = setNode(small_ope, "", 0);
							newTree -> left_child = $3;
							newTree -> right_child = $4;
							$$ = newTree;
						}
						;

    equal               :'(''=' exp exps_equal ')' {
							build_tree* newTree = setNode(eq_ope, "", 0);
							newTree -> left_child = $3;
							newTree -> right_child = $4;
							$$ = newTree;
						}
						;

        exps_equal    	:exp {$$ = $1;}
                        |exps_equal exp {
							build_tree* newTree = setNode(eq_ope, "", 0);
							newTree -> left_child = $1;
							newTree -> right_child = $2;
							$$ = newTree;
						}
                        ;

logical_op         	:and_op {$$ = $1;}
                    |or_op  {$$ = $1;}
                    |not_op {$$ = $1;}
                    ;

    and_op              :'(' and exp exps_and ')' {
							build_tree* newTree = setNode(and_ope, "", 0);
							newTree -> left_child = $3;
							newTree -> right_child = $4;
							$$ = newTree;
						}
						;
						
        exps_and      	:exp {$$ = $1;}
                        |exps_and exp {
							build_tree* newTree = setNode(and_ope, "", 0);
							newTree -> left_child = $1;
							newTree -> right_child = $2;
							$$ = newTree;
						}
                        ;

    or_op               :'(' or exp exps_or ')' {
							build_tree* newTree = setNode(or_ope, "", 0);
							newTree -> left_child = $3;
							newTree -> right_child = $4;
							$$ = newTree;
						}
						;
						
        exps_or        	:exp {$$ = $1;}
                        |exps_or exp {
							build_tree* newTree = setNode(or_ope, "", 0);
							newTree -> left_child = $1;
							newTree -> right_child = $2;
							$$ = newTree;
						}
                        ;

    not_op              :'(' not exp ')' {
							build_tree* newTree = setNode(not_ope, "", 0);
							newTree -> left_child = $3;
							$$ = newTree;
						}
						;

def_stmt           	:'(' define variable exp ')' {
						int tmp = getVarPlace($3->var_name);
						var[tmp].value = getAns($4);
						var[tmp].tree = $4;
					}
					;

    variable            :id {
							int tmp = getVarPlace($1);
							if(tmp == -1){
								var[var_total].var_name = $1;
								var[var_total].value = 0;
								var_total ++;
							}
							
							build_tree* newTree = setNode(var_de, $1, 0);
							$$ = newTree;
						}
						;

fun_exp             :'(' lambda fun_ids fun_body ')' {$$ = $4;}
	         |'(' fun fun_ids fun_body ')' {$$ = $4;}
					;


    fun_ids         	:'(' ')' 		{;}
                        |'(' ids ')'	{;}
                        ;

        ids             :id {
							set_fun($1, fpos);
							fpos ++;
						}
                        |ids id {
							set_fun($2, fpos);
							fpos ++;
						}
                        ;

    fun_body           	:exp {$$ = $1;}

    fun_call            :'(' fun_exp ')' {
							$$ = $2;
						}
                        |'(' fun_exp params ')' {
							get_param($2);
							$$ = $2;
							ppos = 1;
							fpos = 1;
							for(int i=1;i<fun_total;i++)
								fun_var[i].value = 0;
						}
						|'(' fun_name ')' {
							$$ = var[$2].tree;
						}
                        |'(' fun_name params ')' {
							get_param(var[$2].tree);
							$$ = var[$2].tree;
							fpos = 1;
							ppos = 1;
							for(int i=1;i<fun_total;i++)
								fun_var[i].value = 0;
						}
                        ;

        params          :param {
							parameter[ppos] = $1;
							$$ = ppos;
							ppos ++;
						}
                        |params param {
							parameter[ppos] = $2;
							$$ = ppos;
							ppos ++;
						}
                        ;

            param       :exp {$$ = getAns($1);}

    fun_name          	:variable {
							int tmp = -1;
							tmp = getVarPlace($1 -> var_name);
							$$ = tmp;
						}
						;

if_exp             	:'(' IF test_exp then_exp else_exp ')' {
						if(getAns($3) == 0) $$ = $5;
						else $$ = $4;
					}
					;

    test_exp           	:exp {$$ = $1;}
						;

    then_exp          	:exp {$$ = $1;}
						;

    else_exp           	:exp {$$ = $1;}
						;

%%
int main(void){
  yyparse();
  return 0;
}

void yyerror(const char* message){
  if(error == 0){
	  printf("syntax error\n");
	  error = 1;
  }
}

	//create the function that would be used in the grammar
	//create a node
	build_tree *setNode(enum parentType type, char* name, int value){
		build_tree* newTree = (build_tree*) malloc (sizeof(build_tree));
		newTree -> type = type;
		newTree -> var_name = name;
		newTree -> value = value;
		newTree -> left_child = NULL;
		newTree -> right_child = NULL;
		return newTree;
	}
	
	//compute the value of the tree
	int getAns(build_tree *root){
		if(root == NULL) return 0;
		else if(root -> type == var_de) {
			int tmp = getVarPlace(root->var_name);
			return var[tmp].value;
		}
		else if(root -> type == plus_ope) 	return getAns(root->left_child) + getAns(root->right_child);
		else if(root -> type == minus_ope)  return getAns(root->left_child) - getAns(root->right_child);
		else if(root -> type == mul_ope) 	return getAns(root->left_child) * getAns(root->right_child);
		else if(root -> type == div_ope) 	return getAns(root->left_child) / getAns(root->right_child);
		else if(root -> type == mod_ope)	return getAns(root->left_child) % getAns(root->right_child);
		else if(root -> type == and_ope)	return getAns(root->left_child) & getAns(root->right_child);
		else if(root -> type == or_ope) 	return getAns(root->left_child) | getAns(root->right_child);
		else if(root -> type == not_ope)	return !getAns(root->left_child);
		else if(root -> type == eq_ope) 	return getAns(root->left_child) == getAns(root->right_child);
		else if(root -> type == great_ope)  return getAns(root->left_child) > getAns(root->right_child);
		else if(root -> type == small_ope)  return getAns(root->left_child) < getAns(root->right_child);
		else if(root -> type == num_type) 	return root -> value;
		return 0;
	}

	//get the place of the variable
	int getVarPlace(char* name){
		if(var_total==0) return -1;
		for(int i=0;i<var_total;i++){
			//if(strncmp(name, var[i].var_name, strlen(name)) == 0) return i;
			if(strcmp(name, var[i].var_name) == 0) return i;
		}
		return -1;
	}

	//get the place of the function
	int getFunPlace(char* c){
		for(int i=1;i<fun_total;i++) {
			if(strncmp(c, fun_var[i].var_name, strlen(c)) == 0) return i;
		}
		return -1;
	}

	//set the variable of the function
	void set_fun(char* c, int pos){
		int tmp = getFunPlace(c);
		if(tmp == -1){
			fun_var[fun_total].var_name = c;
			fun_var[fun_total].value = pos;
			fun_total ++;
		}
		else {
			fun_var[tmp].value = pos;
		}
	}

	//get the parameter of the function
	void get_param(build_tree* tree){
		if(tree!=NULL) {
			int tmp = getFunPlace(tree -> var_name);
			if(tree -> type == var_de && fun_var[tmp].value != 0){
			
				tree -> type = num_type;
				tree -> value = parameter[fun_var[tmp].value];
				
			}
			
			get_param(tree -> left_child);
			get_param(tree -> right_child);
		}
	}
