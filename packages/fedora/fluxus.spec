%{!?_dist: %{expand: %%define dist fc15}}
%define prever rc1

Summary: A 3D game engine for livecoding worlds into existence
Name: fluxus
Version: 0.18
Release: 2.%{prever}.%{dist}
License: GPLv2
Group: Applications/Multimedia
URL: http://pawfal.org/fluxus/

Source: http://pawfal.org/fluxus/files/fluxus-%{version}.%{prever}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

BuildRequires: ode-devel >= 0.9, racket = 5.1.1, fftw-devel >= 3.2.2, jack-audio-connection-kit-devel >= 1.9.7, libsndfile-devel >= 1.0.25, liblo-devel >= 0.26, glew-devel >= 1.5.8, freetype-devel >= 2.2.4, scons, libjpeg-turbo-devel >= 1.1.1, libpng-devel >= 1.2.46, libtiff-devel >= 3.9.5, zlib-devel >= 1.2.3, freeglut-devel >= 2.6.0, alsa-lib-devel >= 1.0.24, openal-soft-devel >= 1.12.854, gstreamer-devel >= 0.10.25, gstreamer-plugins-base-devel >= 0.10.25, gstreamer-plugins-good-devel >= 0.10.17, gstreamer-plugins-bad-free-devel >= 0.10.22, libunicap-devel >= 0.9.12, ffmpeg-devel >= 0.7
Requires: racket = 5.1.1, fftw >= 3.2.2, jack-audio-connection-kit >= 1.9.7, libsndfile >= 1.0.25, liblo >= 0.26, glew >= 1.5.8, freetype >= 2.2.4, libjpeg-turbo >= 1.1.1, libpng >= 1.2.46, libtiff >= 3.9.5, zlib >= 1.2.3, freeglut >= 2.6.0, alsa-lib >= 1.0.24, openal-soft >= 1.12.854, gstreamer >= 0.10.25, gstreamer-plugins-base >= 0.10.25, gstreamer-plugins-good >= 0.10.17, gstreamer-plugins-bad-free >= 0.10.22, libunicap >= 0.9.12, ffmpeg >= 0.7

%description
A rapid prototyping, livecoding and playing/learning environment for 3D
graphics, sound and games. Extends Racket with graphical commands
and can be used within it’s own livecoding environment or from within
the DrRacket IDE. Web Page: http://www.pawfal.org/fluxus/

%prep
%setup -q -n %{name}-%{version}.%{prever}

%build
cd docs
./makehelpmap.scm

%install
scons -Q install DESTDIR="%{buildroot}" Prefix=/usr RacketPrefix=/usr
install -m 644 -D modules/material/textures/fluxus-icon.png %{buildroot}/usr/share/pixmaps/fluxus-icon.png
install -m 644 -D debian/fluxus.desktop %{buildroot}/usr/share/applications/fluxus.desktop

%post
# fix selinux text relocation errors
chcon -t textrel_shlib_t '/usr/lib/fluxus-018/compiled/native/i386-linux/3m/fluxus-engine_ss.so'
semanage fcontext -a -t textrel_shlib_t '/usr/lib/fluxus-018/compiled/native/i386-linux/3m/fluxus-engine_ss.so'

%clean
%{__rm} -rf %{buildroot}

%files
%defattr(-, root, root, 0755)
%{_bindir}/fluxus
%{_bindir}/fluxa
%{_libdir}/fluxus-018/*
%{_datadir}/fluxus-018/*
%{_docdir}/fluxus-018/*
%{_datadir}/pixmaps/fluxus-icon.png
%{_datadir}/applications/fluxus.desktop

%changelog
* Tue Aug 25 2010 Gabor Papp - 0.18-2.rc1.fc15
- rebuild with ADDONS=1
* Tue Aug 16 2010 Gabor Papp - 0.18-1.rc1.fc15
- rebuild for Fedora Core 15
* Fri Apr 09 2010 Gabor Papp - 0.17-1.rc5.fc12
- 0.17 release candidate 5
* Fri Apr 09 2010 Gabor Papp - 0.17-2.rc4.fc12
- shader crash fix for old graphics cards
* Wed Mar 24 2010 Gabor Papp - 0.17-1.rc4.fc12
- 0.17 release candidate 4
* Tue Mar 23 2010 Gabor Papp - 0.17-1.rc3.fc12
- 0.17 release candidate 3
- added menu entry
* Tue Mar 23 2010 Gabor Papp - 0.17-1.rc2.fc12
- 0.17 release candidate 2
- selinux fix
* Mon Mar 10 2010 Gabor Papp - 0.17-1.20100310git.fc12
- rebuild with ode 0.9 statically
* Mon Mar 08 2010 Gabor Papp - 0.17-1.20100308git.fc12
- rebuild with ode 0.9
* Thu Feb 25 2010 Gabor Papp - 0.17-1.20100225git.fc12
- rebuild for Fedora 12
* Tue Jul 25 2009 Gabor Papp - 0.16-1.20090725git.fc10
- update to git snapshot 2009/07/25
* Tue Jul 23 2009 Gabor Papp - 0.16-1.20090723git.fc10
- update to git snapshot 2009/07/23
* Tue Jul 21 2009 Gabor Papp - 0.16-1.20090721git.fc10
- update to git snapshot 2009/07/21
* Tue Jul 14 2009 Gabor Papp - 0.16-1.20090714git.fc10
- update to git snapshot 2009/07/14
* Sun Jun 12 2009 Gabor Papp - 0.16-1.20090612git.fc10
- update to git snapshot 2009/06/12
* Sun May 16 2009 Gabor Papp - 0.16-1.20090516git.fc10
- update to git snapshot 2009/05/16
* Mon Apr 27 2009 Gabor Papp - 0.16-1.rc1.fc10
- 0.16 release candidate 1
* Wed Apr 15 2009 Gabor Papp - 0.16-1.20090415git.fc10
- update to git snapshot 2009/04/15
* Mon Feb 23 2009 Gabor Papp - 0.16-1.20090223git.fc10
- update to git snapshot 2009/02/23
* Fri Jan 09 2009 Gabor Papp - 0.15-2.20081229git.fc10
- PLT Scheme 4.1.2 version
* Mon Dec 29 2008 Gabor Papp - 0.15-1.20081229git.fc10
- update to git snapshot 2008/12/29
* Mon Dec 01 2008 Gabor Papp - 0.15-1.20081201cvs.fc10
- update to cvs snapshot 2008/12/01
* Sun Nov 30 2008 Gabor Papp - 0.15-1.20081130cvs.fc10
- rebuild for Fedora 10
* Tue Sep 16 2008 Gabor Papp - 0.15-1.20080916cvs.fc8
- update to cvs snapshot 2008/09/16
* Tue Sep 02 2008 Gabor Papp - 0.15-1.20080901cvs.fc8
- update to cvs snapshot 2008/09/01
* Fri Aug 29 2008 Gabor Papp - 0.15-1.20080829cvs.fc8
- update to cvs snapshot 2008/08/29
* Tue Aug 17 2008 Gabor Papp - 0.15-1.20080817cvs.fc8
- update to cvs snapshot 2008/08/17
* Tue Aug 12 2008 Gabor Papp - 0.15-1.20080812cvs.fc8
- update to cvs snapshot 2008/08/12
* Fri Jun 27 2008 Gabor Papp - 0.15-1.20080627cvs.fc8
- update to cvs snapshot 2008/06/27
* Wed Jun 25 2008 Gabor Papp - 0.15-1.20080625cvs.fc8
- update to cvs snapshot 2008/06/25
* Tue Jun 04 2008 Gabor Papp - 0.15-1.20080604cvs.fc8
- fluxus-midi added
- update to cvs snapshot 2008/06/04
* Tue Jun 03 2008 Gabor Papp - 0.15-1.20080603cvs.fc8
- update to cvs snapshot 2008/06/03
* Sun Jun 01 2008 Gabor Papp - 0.15-1.20080601cvs.fc8
- update to cvs snapshot 2008/06/01
* Sun Mar 25 2008 Gabor Papp - 0.15-1.20080525cvs.fc8
- update to cvs snapshot 2008/05/25
* Wed Jan 02 2008 Gabor Papp - 0.14-1.20080101cvs.fc8
- update to cvs snapshot 2008/01/01
* Mon Dec 05 2007 Gabor Papp - 0.14-1.20071201cvs.fc8
- Rebuild for Fedora Core 8
* Mon Dec 03 2007 Gabor Papp - 0.14-1.20071201cvs.fc6
- update to cvs snapshot 2007/12/01
* Thu Nov 22 2007 Gabor Papp - 0.14-1.20071122cvs.fc6
- update to cvs snapshot 2007/11/22
* Sun Nov 18 2007 Gabor Papp - 0.14-1.20071116cvs.fc6
- update to cvs snapshot 2007/11/16
* Wed Oct 31 2007 Gabor Papp - 0.14-1.20071031cvs.fc6
- update to cvs snapshot 2007/10/31
* Tue Oct 28 2007 Gabor Papp - 0.14-1.20071028cvs.fc6
- update to cvs snapshot 2007/10/28
* Thu Oct 25 2007 Gabor Papp - 0.14-1.20071025cvs.fc6
- update to cvs snapshot 2007/10/25
* Wed Oct 24 2007 Gabor Papp - 0.13-1.20071024cvs.fc6
- update to cvs snapshot 2007/10/24
- regenerate helpmap.scm
* Tue Oct 23 2007 Gabor Papp - 0.13-1.20071023cvs.fc6
- update to cvs snapshot 2007/10/23
* Mon Oct 22 2007 Gabor Papp - 0.13-1.20071022cvs.fc6
- update to cvs snapshot 2007/10/22
* Sat Oct 20 2007 Gabor Papp - 0.13-1.20071018cvs.fc6
- update to cvs
* Tue Oct 09 2007 Gabor Papp - 0.13-1.20070904cvs.fc6
- update to cvs
* Tue Oct 09 2007 Gabor Papp - 0.13-1.rc2.fc6
- initial version

