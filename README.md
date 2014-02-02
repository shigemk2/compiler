池袋バイナリ勉強会の資料  #ikebin
===============

# 目的

UNIX V6のコンパイラ、アセンブラを利用して、
コンパイル、アセンブリ、逆アセンブリの仕組みを理解し、
最終的にはOSとコンパイラを自作することを目的とする。

## 概要

https://bitbucket.org/7shi/ikebin/wiki/Home

Wikiの内容を淡々と実行していく

## 自分の開発環境

Mac OS X Mavericks(2014/2/2 現在)

## 環境準備について

↓のリンクを参考に環境は自分で揃えること。

https://bitbucket.org/7shi/i8086tools/wiki/Home

monoを入れたらfsharp系コマンドが利用できるので、
Xamarinは使わずにEmacsとターミナルで作業を進めております。

## ファイルの実行

```
$ fsharpi 1-1.fsx
```

コンパイルなしでも可

## ファイルのコンパイル

```
$ fsharpc 1-1.fsx
```
