function textAreaWC(val, tarID) {
    var len = val.value.length;
    if (len >= 200) {
      val.value = val.value.substring(0, 200);
    } else {
      $('#'+tarID).text(200 - len);
    }
}