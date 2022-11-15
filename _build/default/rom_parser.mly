%token EOF
%token <string*string> KEY_DATA

%start rom
%type <'a list> rom
%%

key_data:
    |keydata = KEY_DATA {keydata}
;

rom : 
    |r = key_data* EOF {r}
    ;