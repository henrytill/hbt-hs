/* Project variables the build stamps into the executable.
 *
 * flake.nix regenerates this header, defining HBT_COMMIT to the revision the
 * executable is built from.  The copy in the repository defines nothing: a
 * build from a source with no git metadata -- a plain `cabal build`, or a Nix
 * build of a tree carrying no revision -- reports the cabal version alone.
 */

#ifndef HBT_PROJECT_H
#define HBT_PROJECT_H

#endif
