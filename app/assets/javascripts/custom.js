var ready = (callback) => {
    if (document.readyState != "loading") callback();
    else document.addEventListener("DOMContentLoaded", callback);
  }

  ready(() => {

    function initCheckboxEventHandlers(checkboxes){

      for(var i = 0; i < checkboxes.length; i++){

        if(isExclusiveCheckbox(checkboxes[i])){

          checkboxes[i].addEventListener("change", handleExclusiveCheckboxSelection);

        } else {

          checkboxes[i].addEventListener("change", handleNonExclusiveCheckboxSelection);

        }
      }

    }

    function handleNonExclusiveCheckboxSelection(event){

      if(event.target.checked){

        var checkboxes = document.getElementsByClassName("govuk-checkboxes__input");

        for(var i = 0; i < checkboxes.length; i++){
          if(isExclusiveCheckbox(checkboxes[i])){
            if(checkboxes[i].checked)checkboxes[i].checked = false;
          }
        }

      }
    }

    function handleExclusiveCheckboxSelection(event){

      if(event.target.checked){

        var checkboxes = document.getElementsByClassName("govuk-checkboxes__input");

        for(var i = 0; i < checkboxes.length; i++){
          if(!isExclusiveCheckbox(checkboxes[i])){
            if(checkboxes[i].checked)checkboxes[i].checked = false;
          }
        }

      }
    }

    function isExclusiveCheckbox(checkbox){
      var exclusive = false;
      if(checkbox.hasAttribute("data-exclusive") && checkbox.getAttribute("data-exclusive") == "true")
        exclusive = true;

      return exclusive;
    }

    var checkboxes = document.getElementsByClassName("govuk-checkboxes__input");

    if(checkboxes.length > 0){

      var foundExclusiveCheckbox = false;
      var counter = 0;

      while(counter < checkboxes.length && !foundExclusiveCheckbox){
        if(isExclusiveCheckbox(checkboxes[counter])){
          foundExclusiveCheckbox = true;
        }
        counter++;
      }

      if(foundExclusiveCheckbox)
        initCheckboxEventHandlers(checkboxes);

    }

  });

