#include <stdio.h>
#include <stdlib.h>
#include "env.h"

struct _env_t {
    int version;
};

env_t *get_env() {
    env_t *env = (env_t *) malloc(sizeof(env_t));
    env->version = 42;
    return env;
}

void display_env(env_t *env) {
    printf("enva: %d\n", env->version);
}

