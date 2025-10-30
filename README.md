# Elite over Econet fileserver menu

This repository contains source code for the Elite over Econet fileserver on the TNMoC Econet Cloud. It uses routines from [Teletext Elite](https://github.com/markmoxon/teletext-elite) to display rotating ships in the BBC Micro's mode 7, and adds a simple menu showing the options available on the server.

![Screenshot of the Elite over Econet fileserver menu](https://elite.bbcelite.com/images/elite_over_econet/fileserver_menu.png)

If you have an Econet network with a Pi Econet Bridge, then you can visit the Elite over Econet fileserver on the TNMoC Econet Cloud like this:

```
*I AM 63.13 BOOT
```

This will run the menu system in this repository. You can find out more about the Elite over Econet fileserver from the menu, or by visiting the [bbcelite.com website](https://elite.bbcelite.com/hacks/elite_over_econet.html).

## Acknowledgements

Elite was written by Ian Bell and David Braben and is copyright &copy; Acornsoft 1984.

The code on this site has been reconstructed from a disassembly of the version released on [Ian Bell's personal website](http://www.elitehomepage.org/).

The commentary and Teletext conversion code are copyright &copy; Mark Moxon. Any misunderstandings or mistakes in the documentation are entirely my fault.

The Teletext routines are by Kieran Connell and Simon Morris of the Bitshifters, and were adapted from Bresenham routines by Rich Talbot-Watkins. See the [Bitshifters teletextr](https://github.com/bitshifters/teletextr/tree/master/lib) repository for the original code.

Huge thanks are due to the original authors for not only creating such an important piece of my childhood, but also for releasing the source code for us to play with; to Paul Brink for his annotated disassembly; and to Kieran Connell for his [BeebAsm version](https://github.com/kieranhj/elite-beebasm), which I forked as the original basis for this project. You can find more information about this project in the [accompanying website's project page](https://elite.bbcelite.com/about_site/about_this_project.html).

The following archive from Ian Bell's personal website forms the basis for this project:

* [BBC Elite, disc version](http://www.elitehomepage.org/archive/a/a4100000.zip)

### A note on licences, copyright etc.

This repository is _not_ provided with a licence, and there is intentionally no `LICENSE` file provided.

According to [GitHub's licensing documentation](https://docs.github.com/en/free-pro-team@latest/github/creating-cloning-and-archiving-repositories/licensing-a-repository), this means that "the default copyright laws apply, meaning that you retain all rights to your source code and no one may reproduce, distribute, or create derivative works from your work".

The reason for this is that Teletext Elite is intertwined with the original Elite source code, and the original source code is copyright. The whole site is therefore covered by default copyright law, to ensure that this copyright is respected.

Under GitHub's rules, you have the right to read and fork this repository... but that's it. No other use is permitted, I'm afraid.

My hope is that the educational and non-profit intentions of this repository will enable it to stay hosted and available, but the original copyright holders do have the right to ask for it to be taken down, in which case I will comply without hesitation. I do hope, though, that along with the various other disassemblies and commentaries of this source, it will remain viable.

---

Right on, Commanders!

_Mark Moxon_
