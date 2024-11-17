+++
title = "OOP has not invented encapsulation"
date = 2024-03-10
draft = false
path = "2024-03/c-encapsulation"

[taxonomies]
categories = ["Software engineering"]
tags = ["design", "engineering", "c"]
+++

Few days ago, I was watching a talk in which the speaker was presenting the
history of the software engineering field of the last fifty years.

At some point he has stated that "OOP introduced (encapsulation)(https://en.wikipedia.org/wiki/Encapsulation_(computer_programming))".

For reference, encapsulation is a technique promoting decouple by
abstracting (hiding) the data structures and algorithms behind interfaces.

At the beginning of my career I was working on an operating system for
[SIM cards](https://en.wikipedia.org/wiki/SIM_card), it was mostly C and Java Card
(a kind of Java 1.4/1.5).

We had to support [processor designs](https://en.wikipedia.org/wiki/Processor_design),
chipsets, and customer customizations.

In order to achieve that, we relied on C headers separation mechanism, let's
say we have an interface which deals with "environment" (whatever it means).

We can have a really simple program:

```c
#include <stdlib.h>
#include "env.h"

int main(void)
{
    env_t *env = get_env();
    display_env(env);
    free(env);
}
```

`env.h` is a basic header file which describes the interface:

```c
#ifndef ENV_H_
#define ENV_H_
typedef struct _env_t env_t;

env_t *get_env();

void display_env(env_t *env);

#endif  /* !ENV_H_ */
```

We have one abstract type `env_t` (`struct _env_t`) and two functions (`get_env`
and `display_env`).

Then we can have two implementations:

```c
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
```

And another more "complex" one:

```c
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
```

We can compile them

```
$ cc -c *.c
```

And finally link them:

```
$ cc -o a main.o enva.o
$ cc -o b main.o envb.o
```

Let's see the results:

```
$ ./a
enva: 42
$ ./b
envb: 4.2
```

Here we are, compile-time (well, linking-time) encapsulation.

On another hand, we can achieve run-time we can rely on function pointers:

```c
typedef struct _env_t {
    void (*display)();
} env_t;
```

Then we can define behaviors:

```c
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
```

Let's use it:

```c
int main(void)
{
    env_t *enva = get_enva();
    enva->display();
    free(enva);

    env_t *envb = get_envb();
    envb->display();
    free(envb);
}
```

```
enva: 42d
envb: 4.2d
```

I think encapsulation is quite old, older than OOP at least.
