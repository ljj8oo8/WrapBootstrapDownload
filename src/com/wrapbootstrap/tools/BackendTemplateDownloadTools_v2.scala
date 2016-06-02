package com.wrapbootstrap.tools

import java.io._
import java.net.{URL}
import java.nio.file.{Files, Paths}

import scala.collection.mutable.HashSet
import scala.io.{Source}
import scala.util.matching.Regex.Match

/**
  * Created by admin on 2016/5/22.
  * html file in  different directory
  *
  */
class BackendTemplateDownloadTools_v2 {

  val baseDir = "G:/Bootstrap/backend/";
  val failureDir = "G:/Bootstrap/";
  def start(url: String) {
    println("--------------------------- " + url + " ---------------------------")
    val baseUrl = url.substring(0, url.lastIndexOf("/") + 1);
    val basePath = baseDir + baseUrl.replace("http://", "").replace("https://", "");
    val homeFile = baseDir + url.replace("http://", "").replace("https://", "");
    writeLog(url, 1);
    if (!Paths.get(basePath).toFile.exists()) {
      Files.createDirectories(Paths.get(basePath));
    }
    val writer = new PrintWriter(homeFile);
    val list = Source.fromURL(url, "UTF-8").getLines().toList;
    list.foreach(writer.write(_));
    writer.close();

    val htmlSet: HashSet[String] = new HashSet();
    val cjjSet: HashSet[String] = new HashSet();
    val fontSet: HashSet[String] = new HashSet();

    list.map(matchLine(_, htmlSet, 0, ""));
    htmlSet.filter((x: String) => x.endsWith(".html") ).map(downLoadHtml(_, baseUrl, basePath, cjjSet));

    println("start download js file  -------------------------------------------------------------------------- js")
    cjjSet.filter((x: String) => x.endsWith(".js") && x.indexOf("http:") < 0 && x.indexOf("https:") < 0).map(downLoadJs(_, baseUrl, basePath));

    println("start download css file  -------------------------------------------------------------------------- cs")
    cjjSet.filter((x: String) => x.endsWith(".css") && x.indexOf("http:") < 0 && x.indexOf("https:") < 0).map(downLoadCss(_, baseUrl, basePath, fontSet))

    println("start download css internal font img file  -------------------------------------------------------------------------- css internal font img")
    val font = (x: String) => if (x.indexOf("?") > 0) x.substring(0, x.indexOf("?")) else if (x.indexOf("#") > 0) x.substring(0, x.indexOf("#")) else x
    fontSet.map(font).filter((x: String) => x.indexOf("data:") < 0 && !x.startsWith("#") && x.indexOf("http:") < 0 && x.indexOf("https:") < 0).map(downloadBinaryFile(_, baseUrl, basePath));

    println("start download img file -------------------------------------------------------------------------- img")
    cjjSet.filter((x: String) => (x.endsWith(".jpg") || x.endsWith(".png")) && x.indexOf("http:") < 0 && x.indexOf("https:") < 0).map(downloadBinaryFile(_, baseUrl, basePath));
  }

  def matchLine(line: String, set: HashSet[String], flag: Int, dir: String) {
    val src = """src="([^"]+)"""".r;
    val href = """href="([^"]+)"""".r;
    val url ="""url\([^(]*\)""".r;
    if (flag == 2) {
      (url findAllMatchIn line).map(_ match { case Match(s) =>
        s.replace("url(", "").replace(")", "").replace("\"", "").replace("'", "")
      })
        .foreach(set add dir + _);
    } else {
      if (flag != 0) {
        (src findAllMatchIn line).map(_ match { case Match(s) =>
          s.replace("src=", "").replace("\"", "")
        }).foreach(set add dir + _);
      }
      (href findAllMatchIn line).map(_ match { case Match(s) =>
        s.replace("href=", "").replace("\"", "")
      }).foreach(set add dir + _);
    }

  }

  def downLoadHtml(fileName: String, baseUrl: String, basePath: String, set: HashSet[String]): Unit = {
    val url = baseUrl + fileName;
    val htmlFile = basePath + fileName;
    val htmlDir=fileName.substring(0, fileName.lastIndexOf("/") + 1);
    if (!Paths.get(htmlFile).getParent.toFile.exists()) {
      Files.createDirectories(Paths.get(htmlFile).getParent);
    }
    val writer = new PrintWriter(htmlFile);
    val bw: BufferedWriter = new BufferedWriter(writer);
    val list = Source.fromURL(url, "UTF-8").getLines().toList;
    list.foreach((x:String)=>{bw.write(x);bw.newLine()});
    bw.close();
    println("suc:" + fileName);
    list.map(matchLine(_, set, 1, htmlDir));
  }

  def downLoadJs(fileName: String, baseUrl: String, basePath: String): Unit = {
    val url = baseUrl + fileName;
    val jsFile = basePath + fileName;
    if (!Paths.get(jsFile).getParent.toFile.exists()) {
      Files.createDirectories(Paths.get(jsFile).getParent);
    }
    val writer = new PrintWriter(jsFile);
    val bw: BufferedWriter = new BufferedWriter(writer);
    try {
      val list = Source.fromURL(url, "UTF-8").getLines().toList;
      list.foreach((x:String)=>{bw.write(x);bw.newLine()});
      bw.close();
      println("suc :" + fileName);
    } catch {
      case ex: java.lang.Exception => println("js file download failure:" + url); writeLog(url, 0);
    }

  }


  def downLoadCss(fileName: String, baseUrl: String, basePath: String, set: HashSet[String]): Unit = {
    val url = baseUrl + fileName;
    val cssFile = basePath + fileName;
    val cssDir = fileName.substring(0, fileName.lastIndexOf("/") + 1);
    if (!Paths.get(cssFile).getParent.toFile.exists()) {
      Files.createDirectories(Paths.get(cssFile).getParent);
    }
    try {
      val writer = new PrintWriter(cssFile);
      val bw: BufferedWriter = new BufferedWriter(writer);
      val list = Source.fromURL(url, "UTF-8").getLines().toList;
      list.foreach((x:String)=>{bw.write(x);bw.newLine()});
      bw.close();
      println("css file suc :" + fileName);
      list.map(matchLine(_, set, 2, cssDir));
    } catch {
      case ex: java.lang.Exception => println(" css file download failure:" + url); writeLog(url, 0);
    }

  }


  def downloadBinaryFile(fileName: String, baseUrl: String, basePath: String): Unit = {
    val url = baseUrl + fileName;
    val imgFile = basePath + fileName;
    if (!Paths.get(imgFile).getParent.toFile.exists()) {
      Files.createDirectories(Paths.get(imgFile).getParent);
    }
    val u: URL = new URL(url);
    try {
      val dataInputStream: DataInputStream = new DataInputStream(u.openStream());
      val fileOutputStream: FileOutputStream = new FileOutputStream(new File(imgFile));
      val buffer: Array[Byte] = new Array[Byte](1024);
      var length: Int = 0;
      while ( {
        length = dataInputStream.read(buffer); length > 0
      }) {
        fileOutputStream.write(buffer, 0, length);
      }
      dataInputStream.close();
      fileOutputStream.close();
      println("binary file suc :" + fileName);
    } catch {
      case ex: java.lang.Exception => println("failure:" + url); writeLog(url, 0);
    }
  }

  def writeLog(log: String, flag: Int): Unit = {
    val writer: FileWriter = new FileWriter(failureDir + "failure.txt", true);
    val bw: BufferedWriter = new BufferedWriter(writer);
    bw.newLine();
    if (flag == 0) {
      bw.write("        --" + log);
    } else {
      bw.write(log);
    }
    bw.close();
  }


}

object Testt {
  def main(args: Array[String]) {
    var list = List[String](); //40

    //__list+:="http://infinite-woodland-5276.herokuapp.com/index.html"//20 socket reset
    //__list +:= "http://seantheme.com/color-admin-v2.0/admin/ajax/index.html"
    //__list+:="http://192.241.236.31/themes/preview/smartadmin/1.8.x/ajax/index.html"//40
    //__list+:="http://radmin.squareturtle.com/1.1.0/index.html"//30
    //__list+:="https://jumpstartthemes.com/demo/v/2.1.0/templates/admin/index.html"//15
    //__list+:="http://responsiweb.com/themes/preview/ace/1.4/index.html"//10
    //__list +:= "http://seantheme.com/color-admin-v2.0/admin/html/index_v2.html" //30

    /*
        list+:="http://webapplayers.com/inspinia_admin-v2.5/index.html" //40


        list+:="http://themicon.co/theme/angle/v3.3.1/backend-jquery/app/dashboard.html"//10 有其他版本
        list+:="http://ashobiz.asia/mac53/macadmin/index.html"//10
        list+:="http://detail.herokuapp.com/index.html"//10
        list+:="http://webapplayers.com/homer_admin-v1.9/index.html"//20
        list+:="http://jumpstartthemespreview.com/base-admin-3.0/index.html"//5
        list+:="http://foxythemes.net/cleanzone/index.html"//10
        list +:="http://beer2code.com/themes/core-admin-3/pages/dashboard/dashboard.html" //5 需要改造html路径
        list +:="http://demo.flatlogic.com/3.3.1/dark/index.html" //15  light|dark
        list+:="http://foxythemes.net/cleanzone/index.html"
        list+:="http://www.themeon.net/nifty/v2.4.1/index.html"
        list+:="http://byrushan.com/projects/ma/1-6-1/jquery/dark/index.html"
        list+:="http://byrushan.com/projects/ma/1-6-1/jquery/light/index.html"*/

    list.map(new BackendTemplateDownloadTools_v2().start(_))


  }
}
