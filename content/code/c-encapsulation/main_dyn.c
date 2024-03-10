#include <stdio.h>
#include <stdlib.h>

typedef struct _env_t {
    void (*display)();
} env_t;

void display_enva() {
    printf("enva: 42d\n");
}

env_t *get_enva() {
    env_t *env = (env_t *) malloc(sizeof(env_t));
    env->display = &display_enva;
    return env;
}

void display_envb() {
    printf("envb: 4.2d\n");
}

env_t *get_envb() {
    env_t *env = (env_t *) malloc(sizeof(env_t));
    env->display = &display_envb;
    return env;
}

int main(void)
{
    env_t *enva = get_enva();
    enva->display();
    free(enva);

    env_t *envb = get_envb();
    envb->display();
    free(envb);
}
