{ inputs, lib, config, pkgs, ... }: {

  imports = [];

  programs.gpg = {
    enable = true;
    homedir = "${config.xdg.configHome}/gnupg";
    mutableKeys = false;
    mutableTrust = false;
    publicKeys = [{ source = ../users/zamlz/public.key; trust = 5; }];
    settings = {
      default-key = "0FA6 FF80 89E9 C767 0A22  54C7 9731 7FD0 FC2D B3CC";
      keyid-format = "0xlong";
    };
  };

  services.gpg-agent = {
    enable = true;
    enableScDaemon = true;
    enableSshSupport = true;
    enableZshIntegration = true;
    defaultCacheTtl = 600;
    defaultCacheTtlSsh = 600;
    # FIXME: Why does this not work?
    # extraConfig = ''
    # fixed-list-mode
    # keyid-format 0xlong
    # with-fingerprint
    # personal-digest-preferences SHA512 SHA384 SHA256 SHA224
    # default-preference-list SHA512 SHA384 SHA256 SHA224 AES256 AES192 AES CAST5 BZIP2 ZLIB ZIP Uncompressed
    # use-agent
    # verify-options show-uid-validity
    # list-options show-uid-validity
    # cert-digest-algo SHA256
    # no-emit-version
    # '';
    maxCacheTtl = 7200;
    maxCacheTtlSsh = 7200;
    pinentryFlavor =  "tty"; # FIXME: use custom pinentry?
    sshKeys = [ "FA508B6D901BA2A59DE2B7E521EBE58F4CDD6C0D" ];
  };
}
