# 配置安装

通过命令行进行安装，把以下代码粘贴到终端中运行即可：

```bash
git clone https://github.com/cabins/.emacs.d ~/.emacs.d
```

如果你使用的是 27+版本，你也可以运行以下代码来安装：

```bash
git clone https://github.com/cabins/.emacs.d ~/.config/emacs
```

> 注意: 如果你使用的是 Windows 平台的话，你需要自行设置一个 HOME 环境变量，否则默认安装到`%AppData%`下。

## 问题排查

> Windows上如果出现闪屏

如果你在Windows 10上发现界面存在闪屏的情况（比如移动光标的时候，或者键入的时候），请检查是否开启了MacType。如果是的话，将Emacs的进程添加到MacType的排除列表中即可，例如在MacType的ini文件中添加如下的代码：
```ini
[UnloadDll]
emacs.exe
runemacs.exe
```

> 如果出现乱码

请使用`all-the-icon`进行字体的补全安装。另外如果是Windows的话，请额外安装Symbola。
