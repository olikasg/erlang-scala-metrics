#ifndef __suffix_tree_h_
#define __suffix_tree_h_

extern "C" {
    #include "erl_nif.h"
    
    int load(ErlNifEnv* env, void**, ERL_NIF_TERM load_info);
    int reload(ErlNifEnv* env, void**, ERL_NIF_TERM load_info);
    int upgrade(ErlNifEnv* env, void**, void**, ERL_NIF_TERM load_info);
    void unload(ErlNifEnv*, void*);

    ERL_NIF_TERM search_dupcode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
}
#endif
