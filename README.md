# Compiler course

Cool programming language compiler

### Resources

* [student-dist](https://s3-us-west-1.amazonaws.com/prod-edx/Compilers/Misc/student-dist.tar.gz) which is included here.

## Compiler local development

* Start the container with Docker Compose:

```bash
$ docker-compose up -d
```

**Note**: The first time you start your stack, it might take a minute for it to be ready.

To get inside the container with a `bash` session you have to `exec` inside the running container:

```console
$ docker-compose exec cool bash
```

You should see an output like:

```console
root@ebdeb9f18e65:/cool#
```

that means that you are in a `bash` session inside your container, as a `root` user, under the `/cool` directory.
