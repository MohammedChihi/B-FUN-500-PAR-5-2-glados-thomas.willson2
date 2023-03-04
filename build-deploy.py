#!/usr/bin/env python3

import subprocess


subprocess.run(["docker", "build", "-t", "glado_images", "."])


subprocess.run(["docker", "login", "rg.fr-par.scw.cloud/", "-u", "nologin", "-p", "1f7c7b1d-2dee-4122-bf2e-3dc6e637d545"])


subprocess.run(["docker", "tag", "glado_image", "rg.fr-par.scw.cloud/glados-registry/glado_image"])


subprocess.run(["docker", "push", "rg.fr-par.scw.cloud/glados-registry/glado_image"])