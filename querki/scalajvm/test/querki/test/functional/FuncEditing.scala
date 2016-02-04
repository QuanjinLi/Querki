package querki.test.functional

/**
 * Editor-specific functions. These are broken into their own layer because there are so many of them.
 * 
 * @author jducoeur
 */
trait FuncEditing { this:FuncMixin =>
  
  def chooseModel(thing:TInstance) = {
    waitFor("_modelSelected")
    singleSel("_modelSelector").value = thing.model.tid.underlying
    click on "_modelSelected"    
  }
  
  def editorId(thing:TThing[_], prop:TProp):String = s"v-${prop.oidStr}-${thing.oidStr}"
  
  // TODO: I'm *sure* that this should be done with a typeclass instead, but it's kicking my butt, so
  // we're just going to do it the primitive way for now.
  def setValue(thing:TThing[_], prop:TProp, v:String) = {
    prop.tpe match {
      case TTextType => textField(editorId(thing, prop)).value = v
      case _ => fail(s"Don't yet know how to set values for properties of type ${prop.tpe}")
    }
  }
  
  /**
   * Design a new Model.
   * 
   * @param ops Any number of operations to perform inside the Advanced Editor. Setting the Name is
   *   automatic, but everything else needs to be specified. 
   */
  def designAModel(thing:TInstance, ops:State => State*)(state:State):State = {
    spew(s"Designing Model ${thing.display}")
    // Choose "Design a Model" from the menu, and select the correct Model from the popup dialog:
    clickMenuItem(DesignModelItem)
    chooseModel(thing)
    
    // Fill out the Model Designer, based on the specified properties:
    val page = ModelDesigner(thing)
    // Note that this page sometimes uses thingTitle instead, but shouldn't when we get to it from here:
    val expectedTitle = page.msg("pageTitle", ("modelName" -> ""))
    // Note that we have to use include instead of be, because we haven't set the name yet, so it's
    // going to use the TID instead:
    eventually { pageTitle should include (expectedTitle) }
    // Note the explicit assumption that the TID is at the end of the title here. That's suspicious, and
    // may not survive in the long run.
    val instanceTID = pageTitle.drop(expectedTitle.length)
    val thingWithTID = thing.withTID(instanceTID).copy(isModel = true)
    val stateWithThing = state.updateSpace(space => space.copy(things = space.things :+ thingWithTID))
    setValue(thingWithTID, NameProp, thing.display)
    val stateAfterOps = run(stateWithThing, ops:_*)
    click on "_doneDesigning"
    waitUntilCreated(thing)
    stateAfterOps
  }
  
  /**
   * Creates an Instance, using the Create any Thing menu pick.
   */
  def createAnyThing(thing:TInstance)(state:State):State = {
    spew(s"Creating Instance ${thing.display}")
    // Choose "Create any Thing" from the menu, and select the correct Model from the popup dialog:
    clickMenuItem(CreateThingItem)
    chooseModel(thing)
    
    // Fill out the CreateAndEdit page:
    waitForTitle(CreateAndEdit(thing.model))
    val editor = find(className("_instanceEditor")).getOrElse(fail("Couldn't find instance editor for newly-created Thing!"))
    val instanceTID = editor.attribute("data-thingid").getOrElse(fail("Couldn't find TID for newly-created Thing!"))
    val thingWithTID = thing.withTID(instanceTID)
    // Fill in the name property:
    setValue(thingWithTID, NameProp, thing.display)
    
    // Click "Done", and update the State with the newly-created Thing:
    click on "_editDoneButton"
    val t = waitUntilCreated(thingWithTID)
    state.updateSpace(space => space.copy(things = space.things :+ t))
  }
}
