# Installation guide

At the moment, there are only installation instructions for Ubuntu.
It should be easy to adapt these instructions for other Linux systems.

On a non-Linux OS, I recommend to install AERN2 in a
small Ubuntu VM.  The main obstacle I hit when installing
natively on other OS is linking Haskell with MPFR.

## From latest sources on Ubuntu using stack

  * Install stack (Haskell build tool).

    * Use `curl` or `wget` following the [stack installation instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

    * Add `/usr/local/bin` to your path or create a symbolic link to `/usr/local/bin/stack` somewhere on your path, eg as follows:

      `cd; mkdir -p bin; ln -s /usr/local/bin/stack bin/stack`

    * Check stack is installed by running:

      `stack --version`

        * This should report version 1.9 or newer.

  * Setup stack and install [ghc](https://www.haskell.org/ghc/) (Haskell compiler) 8.4.4 (unless already installed).

    ```
    sudo apt-get install build-essential
    sudo apt-get install libmpfr-dev
    stack setup --resolver lts-12.17
    ```

      * This command is likely to take several minutes to complete and download a large amount of data.

      * Prefer another stack resolver?  AERN2 is likely to work with other resolvers, but beware that in ghc 8.0.2 AERN2 does not work with ghci due to a ghci bug.

    * Check your installation by running:

      `stack ghc -- --version`

      * This should report ghc version 8.4.4.

  * Clone aern2 git repository as follows:

    * Create a new folder (from now on called the "base folder") and in this folder run:

      ```
      git clone https://github.com/michalkonecny/aern2.git
      ```

  * Build the packages as follows:

    * Copy stack.yaml to the base folder:

      `cp aern2/docs/install/stack.yaml-ghc8.4.4 stack.yaml`

    * Run `stack install` in the base folder.

      * If successful, you should see something like:

        ```
        Copied executables to /home/.../.local/bin:
        - aern2-real-benchOp
    
  * Run the test suite using `stack test aern2-real`
    ```
