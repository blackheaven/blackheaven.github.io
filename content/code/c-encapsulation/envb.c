#include <stdio.h>
#include <stdlib.h>
#include "env.h"

struct _env_t {
    int version_major;
    int version_minor;
};

env_t *get_env() {
    env_t *env = (env_t *) malloc(sizeof(env_t));
    env->version_major = 4;
    env->version_minor = 2;
    return env;
}

void display_env(env_t *env) {
    printf("envb: %d.%d\n", env->version_major, env->version_minor);
}

