#include <stdlib.h>
#include "env.h"

int main(void)
{
    env_t *env = get_env();
    display_env(env);
    free(env);
}
