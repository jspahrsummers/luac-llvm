%lua_State = type opaque
%lua_Number = type double
%lua_Integer = type i64
%lua_Unsigned = type i32

declare %lua_State* @luaL_newstate ()
%luaL_newstate_fp = type %lua_State* ()*

declare void @lua_pushnumber (%lua_State*, %lua_Number)
%lua_pushnumber_fp = type void (%lua_State*, %lua_Number)*

@globalState = private unnamed_addr global %lua_State* null

define i32 @main () {
entry:
    %state = call %luaL_newstate_fp @luaL_newstate ()
    store %lua_State* %state, %lua_State** @globalState

    ; generated code starts here
