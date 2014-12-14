lisp-mode-extra
===============
lisp-mode に追加する雑多なものたち。

詳細な説明
----------
以下のコマンドやら何やらが含まれています。

* `(` を入力時にカッコの対応を見て `)` を挿入したりしなかったり
* `)` を入力時にカッコの対応を見てスキップしたり
* `"` を入力時に文字列の状態を見て閉じたりエスケープしたり
* インデント計算の変更
  * 基本は標準と同じ
  * `ed::lisp-indent-clause` プロパティで何かする（後述）
* lisp ファイルを保存時にコンパイルやロード

### `ed::lisp-indent-clause` のインデント

	(setf (get 'SYMBOL 'ed::lisp-indent-clause) N)  ; N は数
	(setf (get 'SYMBOL 'ed:lisp-indent-hook) 1)

としておくと、`SYMBOL` をオペレータとする式の N 番目以降の引数式が暗黙の
`progn` のようなカタチになります。

	(SYMBOL
	    (some stuff comes here...) ;N番目より前の式
	  ;; N番目以降の式
	  (clause comes here and
	    (its "body" forms comes here)
	    ...)
	  (more clauses...))

xyzzy 標準では `handler-case` のみがこのインデントになっていますが、それ
を他のオペレータでも利用できるようにしたものです。



使い方など
----------

### 必要なもの
* xyzzy version 0.2.2.242 以降（`si:*function-name*`）

### インストール＆設定
作りかけですけど使ってみたい人は、とりあえずクローンして

	% cd ${SITE_LISP}
	% git clone https://github.com/bowbow99/xyzzy.lisp-mode-extra.git lisp-mode-extra

`.xyzzy` などで

	(pushnew "${SITE_LISP}/lisp-mode-extra" *load-path* :test #'path-equal)
	(require "lisp-mode-extra")
	(lisp-mode-extra-setup)

上記の設定をしておけば `lisp-mode` や `lisp-interaction-mode` で有効になります。


その他
------

### バグ報告、要望、質問など
* [Github Issues](https://github.com/bowbow99/xyzzy.lisp-mode-extra/issues)
* [bowbow99 のツイッター](https://twitter.com/bowbow99)
* [bowbow99 のはてダ](http://d.hatena.ne.jp/bowbow99)
* 2ch の xyzzy part.# にカキコ
* 自分のブログに書いておく
* 紙に書いて瓶に詰めて海へ流す

### 作った人（たち）

* [bowbow99](https://github.com/bowbow99)

### ライセンス

Copyright (c) 2014 bowbow99

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
