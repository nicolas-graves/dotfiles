# Recursive Emacs hot-reload example

After applying the Home configuration:

```sh
herd start recursive-emacs-text
emacsclient "$PWD/examples/emacs-hot-reload/example.txt"
```

Edit and save `example.txt`. The Shepherd watcher asks the running Emacs to
append ` a` to the buffer without saving it. Each subsequent manual save
writes the previous append and triggers one more append, so the recursion is
explicitly paced by saves rather than becoming an uncontrolled event loop.

Stop it with:

```sh
herd stop recursive-emacs-text
```
