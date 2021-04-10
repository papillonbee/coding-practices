package coding_practices.cache

import coding_practices.generator.Generator
import coding_practices.model.Dinosaur
import coding_practices.model.stubber.DinosaurStubber
import org.mockito.Matchers.any
import org.mockito.Mockito.{never, times, verify, when}
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

import java.util.UUID

class LeastRecentlyUsedCacheTest extends AnyFunSpec
  with Matchers
  with OptionValues
  with MockitoSugar {

  trait DummyDinosaurService {
    def fetchDinosaurById(id: UUID): Option[Dinosaur]
  }

  describe("LeastRecentlyUsedMemoryCache") {
    val maximumSize: Int = 3
    val lruCached: LeastRecentlyUsedCache[UUID, Dinosaur] =
      new LeastRecentlyUsedCacheImpl[UUID, Dinosaur](maximumSize)

    val dinosaurService: DummyDinosaurService = mock[DummyDinosaurService]

    it("should get value by key from cache if exist") {
      val dinosaurId: UUID = Generator.uuid.build
      val dinosaur: Dinosaur = DinosaurStubber.builder
        .name("John")
        .age(22)
        .build

      lruCached.put(dinosaurId, dinosaur)

      val cachedDinosaur: Option[Dinosaur] = lruCached.get(
        dinosaurId, () => dinosaurService.fetchDinosaurById(dinosaurId)
      )

      cachedDinosaur.value shouldEqual dinosaur
      verify(dinosaurService, never()).fetchDinosaurById(any[UUID])
    }

    it("should get value by key from the supplied callable if not exist in the cache") {
      val dinosaurId: UUID = Generator.uuid.build
      val dinosaur: Dinosaur = DinosaurStubber.builder
        .name("John")
        .age(22)
        .build

      val fetchedDinosaur: Option[Dinosaur] = Generator.option(dinosaur).build
      when(dinosaurService.fetchDinosaurById(dinosaurId)).thenReturn(fetchedDinosaur)

      val cachedDinosaur: Option[Dinosaur] = lruCached.get(
        dinosaurId, () => dinosaurService.fetchDinosaurById(dinosaurId)
      )

      cachedDinosaur shouldEqual fetchedDinosaur
      verify(dinosaurService, times(1)).fetchDinosaurById(dinosaurId)
    }
  }
}
