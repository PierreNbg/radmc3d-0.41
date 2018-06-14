import numpy as np
import matplotlib.pyplot as plt
from radmc3dPy.analyze import *

o = readOpac(ext=['mix'],scatmat=[True])

plt.figure()
plt.plot(o.wav[0],o.kabs[0],label=r'$\kappa_{a}$ (absorption)')
plt.plot(o.wav[0],o.ksca[0],label=r'$\kappa_{s}$ (scattering)')
plt.xscale('log')
plt.yscale('log')
plt.ylim(ymin=1e-2)
plt.xlabel(r'$\lambda [\mu\mathrm{m}]$')
plt.ylabel(r'$\kappa [\mathrm{cm}^2/\mathrm{g}]$')
plt.legend()

ilam = [76,175,250,369]
lamn = [r'$\lambda=0.2\,\mu\mathrm{m}$',r'$\lambda=0.5\,\mu\mathrm{m}$',r'$\lambda=1\,\mu\mathrm{m}$',r'$\lambda=3\,\mu\mathrm{m}$']
plt.figure()
for i in range(len(ilam)):
    plt.plot(o.scatang[0],4*np.pi*o.z11[0][ilam[i]]/o.ksca[0][ilam[i]],label=lamn[i])
#plt.yscale('log')
plt.xlabel(r'$\theta [\mathrm{deg}]$')
plt.ylabel(r'$4\pi\,Z_{11}/\kappa_{s}$')
plt.legend()
plt.show()
